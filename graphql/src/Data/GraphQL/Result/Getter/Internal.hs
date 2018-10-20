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

module Data.GraphQL.Result.Getter.Internal where

import Control.Applicative (many, (<|>))
import Control.Monad (unless, void)
import Data.Aeson (Value(..))
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Data.Typeable (typeRep)
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (getQ, lift, putQ)
import Text.Megaparsec (Parsec, eof, parseErrorPretty, runParser)
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
generateGetter resultCon fullSchema input = do
  GetterExpr{..} <- parse getterExpr input

  -- `startVar` is the local variable being queried
  -- `resultVar` is the `Value` created/extracted from `start`
  -- Need to prevent name clash between `startVar` and `resultVar`
  let startVar = mkName start
  resultVar <- newName $ case start of
    "result" -> "_result"
    _ -> "result"

  getterData <- fromMaybe [] <$> getQ :: Q GetterData
  let varSchema = case (toStoreName start `lookup` getterData, useSchema) of
        (Nothing, False) -> Nothing
        (Just schema, True) -> Just schema
        (Nothing, True) -> error $ "Schema is not stored for " ++ start
        (Just _, False) -> error $ "Did you intend to use `@" ++ start ++ "` instead?"
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
    Just name -> do
      let storeName = toStoreName name
      case filter ((== storeName) . fst) getterData of
        [] -> putQ $ (storeName, getObjectSchema finalSchema) : getterData
        [(_, schema)] -> unless (schema == finalSchema) $
          error $ "Another schema is already stored for " ++ name
        _ -> error $ "Multiple schema stored for " ++ name

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
mkGetter :: [GetterOperation] -> Schema -> (ExpQ, Schema)
mkGetter [] schema = (getFinalizer schema, schema)
mkGetter (op:ops) schema = case op of
  GetterKey key ->
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
  , useSchema   :: Bool
  , getterOps   :: [GetterOperation]
  , storeSchema :: Maybe String
  } deriving (Show)

data GetterOperation
  = GetterKey String
  | GetterBang
  | GetterList
  deriving (Show)

type Parser = Parsec Void String

identifier :: Parser String
identifier = (:) <$> lowerChar <*> many (alphaNumChar <|> char '\'')

getterExpr :: Parser GetterExpr
getterExpr = do
  space
  useSchema <- (string "@" $> True) <|> pure False
  start <- identifier
  getterOps <- many getterOp
  space
  storeSchema <- (string ">" *> space *> fmap Just identifier) <|> pure Nothing
  space
  void eof
  return GetterExpr{..}

getterOp :: Parser GetterOperation
getterOp = parseGetterKey <|> parseGetterBang <|> parseGetterList
  where
    parseGetterKey = GetterKey <$> (string "." *> identifier)
    parseGetterBang = string "!" $> GetterBang
    parseGetterList = string "[]" $> GetterList
