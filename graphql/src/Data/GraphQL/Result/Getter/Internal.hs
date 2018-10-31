{-|
Module      :  Data.GraphQL.Result.Getter.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal definitions for Data.GraphQL.Result.Getter, separated to allow testing. These functions
should NOT be used directly otherwise.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.GraphQL.Result.Getter.Internal where

import Control.Applicative (many, (<|>))
import Control.Monad (unless, void)
import Data.Aeson (Value(..))
import Data.Functor (($>))
import Data.List.Extra (allSame)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Data.Typeable (typeRep)
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Text.Megaparsec
    (Parsec, between, choice, eof, option, parseErrorPretty, runParser, sepBy1)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space, string)
import TH.Utilities (proxyE, typeRepToType)

import Data.GraphQL.Result.Aeson
import Data.GraphQL.Result.Schema (Schema(..), getEnum)

type GetterData = [(String, Schema)]

-- | Generate the let-expression for a getter quasiquoter.
--
-- [get| result.foo |], where `foo` is `SchemaText`, generates:
--
-- @
-- let UnsafeResult _result = result
-- in getText . getKey "foo" $ _result
-- @
--
-- If a schema is already stored for the name `node`, [get| @node.bar[].baz |], where `bar` is
-- `SchemaList SchemaObject` and `baz` is `SchemaInt`, generates:
--
-- @
-- let result = Object node
-- in ((getInt . getKey "baz") `mapList`) . getKey "bar" $ result
-- @
--
-- These expressions are partial (all the `get*` functions), but since we typecheck the schemas,
-- they should be type-safe.
generateGetter :: Name -> Schema -> String -> ExpQ
generateGetter = generateGetter' TH.getQ TH.putQ

generateGetter' :: Q (Maybe GetterData) -> (GetterData -> Q ()) -> Name -> Schema -> String -> ExpQ
generateGetter' getQ putQ resultCon fullSchema input = do
  GetterExpr{..} <- parse getterExpr input

  -- `startVar` is the local variable being queried
  -- `resultVar` is the `Value` created/extracted from `start`
  -- Need to prevent name clash between `startVar` and `resultVar`
  let startVar = mkName start
  resultVar <- newName $ case start of
    "result" -> "_result"
    _ -> "result"

  getterData <- fromMaybe [] <$> getQ
  let varSchema = case useSchema of
        Nothing ->
          case toStoreName start `lookup` getterData of
            Nothing -> Nothing
            Just _ -> error $ "Did you intend to use `@" ++ start ++ "` instead?"
        Just schemaName ->
          case toStoreName schemaName `lookup` getterData of
            Nothing -> error $ "Schema is not stored for " ++ start
            Just schema -> Just schema
      initialSchema = fromMaybe fullSchema varSchema
      (getterFunc, finalSchema) = mkGetter getterOps initialSchema
      letDecl = if isNothing varSchema
        -- let UnsafeResult resultVar = startVar
        then valD (conP resultCon [varP resultVar]) (normalB $ varE startVar) []
        -- let resultVar = Object startVar
        else valD (varP resultVar) (normalB [| Object $(varE startVar) |]) []
      -- in ... $ result
      letExpr = appE getterFunc $ varE resultVar

  case storeSchema of
    Nothing -> return ()
    Just name ->
      case finalSchema of
        Just finalSchema' -> do
          let storeName = toStoreName name
          case filter ((== storeName) . fst) getterData of
            [] -> putQ $ (storeName, getObjectSchema finalSchema') : getterData
            [(_, schema)] -> unless (schema == finalSchema') $
              error $ "Another schema is already stored for " ++ name
            -- unreachable
            _ -> error $ "Multiple schema stored for " ++ name
        Nothing -> error $ "Unable to store schema after: " ++ show (last getterOps)

  letE [letDecl] letExpr
  where
    parse p s = either (fail . parseErrorPretty) return $ runParser p s s
    toStoreName name = show resultCon ++ "$" ++ name
    getObjectSchema = \case
      SchemaMaybe inner -> getObjectSchema inner
      SchemaList inner -> getObjectSchema inner
      schema@(SchemaObject _) -> schema
      schema -> error $ "Cannot store schema " ++ show schema

-- | Make the getter function, iterating over each operation and traversing the given schema.
--
-- Returns the getter function and the final schema leftover from the traversal.
mkGetter :: [GetterOperation] -> Schema -> (ExpQ, Maybe Schema)
mkGetter [] schema = (getFinalizer schema, Just schema)
mkGetter (op:ops) schema = case op of
  GetterKey key -> getterKey key
  GetterKeyList keys ->
    let (getterKeys, schemas) = unzip $ map getterKey keys
        val = mkName "v"
        appliedKeys = map (`appE` varE val) getterKeys
    in if allSame schemas
      then (lamE [varP val] $ listE appliedKeys, Nothing)
      else error $ "List must contain all the same types: " ++ show schemas
  GetterKeyTuple keys ->
    let getterKeys = map (fst . getterKey) keys
        val = mkName "v"
        appliedKeys = map (`appE` varE val) getterKeys
    in (lamE [varP val] $ tupE appliedKeys, Nothing)
  GetterBang ->
    case schema of
      SchemaMaybe inner -> withNext ops inner $ \f -> [| $f . fromJust |]
      _ -> error $ "Cannot use the '!' operator on the schema " ++ show schema
  GetterList ->
    case schema of
      SchemaList inner -> withNext ops inner $ \f -> [| ($f `mapList`) |]
      SchemaMaybe inner -> withNext (op:ops) inner $ \f -> [| ($f `mapMaybe`) |]
      _ -> error $ "Cannot use the '[]' operator on the schema " ++ show schema
  where
    withNext ops' schema' mkExpr =
      let (f, finalSchema) = mkGetter ops' schema'
      in (mkExpr f, finalSchema)
    getterKey key =
      case schema of
        SchemaObject fields ->
          case Text.pack key `lookup` fields of
            Just field ->
              let fromKey = case field of
                    SchemaMaybe _ -> [| lookupKey |]
                    _ -> [| getKey |]
              in withNext ops field $ \f -> [| $f . $fromKey $(lift key) |]
            Nothing -> error $
              "Invalid key: " ++ key ++ ". Possible keys: " ++
              Text.unpack (Text.intercalate ", " $ map fst fields)
        SchemaMaybe inner -> withNext (op:ops) inner $ \f -> [| ($f `mapMaybe`) |]
        _ -> error $ "Cannot get key '" ++ key ++ "' at schema " ++ show schema

-- | Get the final function to parse a value with the given schema.
getFinalizer :: Schema -> ExpQ
getFinalizer = \case
  SchemaBool -> [| getBool |]
  SchemaInt -> [| getInt |]
  SchemaDouble -> [| getDouble |]
  SchemaText -> [| getText |]
  SchemaScalar -> [| getScalar |]
  SchemaEnum proxy ->
    let proxyType = typeRep proxy
    in [| getEnum $(proxyE $ typeRepToType proxyType) . getText |]
  SchemaMaybe inner -> [| mapMaybe $(getFinalizer inner) |]
  SchemaList inner -> [| mapList $(getFinalizer inner) |]
  SchemaObject _ -> [| getObject |]

{- Parser for getter quasiquotes -}

data GetterExpr = GetterExpr
  { start       :: String
  , useSchema   :: Maybe String
  , getterOps   :: [GetterOperation]
  , storeSchema :: Maybe String
  } deriving (Show)

data GetterOperation
  = GetterKey String
  | GetterKeyList [String]
  | GetterKeyTuple [String]
  | GetterBang
  | GetterList
  deriving (Show)

type Parser = Parsec Void String

identifier :: Parser String
identifier = (:) <$> lowerChar <*> many (alphaNumChar <|> char '\'')

getterExpr :: Parser GetterExpr
getterExpr = do
  space
  (start, useSchema) <- getStart
  getterOps <- many getterOp
  space
  storeSchema <- option Nothing $ string ">" *> space *> fmap Just identifier
  space
  void eof
  return GetterExpr{..}

-- | Gets the starting identifier and possibly the stored schema to start with.
--
-- One of:
--   * `var`      -> ("var", Nothing)
--   * `@var`     -> ("var", Just "var")
--   * `@tag var` -> ("var", Just "tag")
getStart :: Parser (String, Maybe String)
getStart = (string "@" *> getStartWithSchema) <|> fmap (, Nothing) identifier
  where
    getStartWithSchema = do
      ident <- identifier
      fmap (, Just ident) $ option ident $ space *> identifier

getterOp :: Parser GetterOperation
getterOp = choice
  [ string "." *> choice
      [ fmap GetterKey identifier
      , fmap GetterKeyList $ between (string "[") (string "]") $ identifier `sepBy1` string ","
      , fmap GetterKeyTuple $ between (string "(") (string ")") $ identifier `sepBy1` string ","
      ]
  , string "!" $> GetterBang
  , string "[]" $> GetterList
  ]
