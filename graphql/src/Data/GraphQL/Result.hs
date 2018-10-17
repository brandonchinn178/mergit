{-|
Module      :  Data.GraphQL.Result
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for defining schemas and querying GraphQL results.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GraphQL.Result
  ( GraphQLResult
  , getErrors
  , getResult
  , Schema(..)
  , getterFor
  , module Result
  -- * Re-exports
  , QuasiQuoter
  , Value
  ) where

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Aeson (FromJSON(..), Value(..), withObject, (.!=), (.:?))
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (getQ, lift, putQ)
import Text.Megaparsec (Parsec, eof, parseErrorPretty, runParser)
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space, string)
import TH.Utilities (proxyE, typeRepToType)

import Data.GraphQL.Error (GraphQLError)
import Data.GraphQL.Result.Parse as Result

-- | A result of a GraphQL query.
data GraphQLResult r = GraphQLResult
  { resultErrors :: [GraphQLError]
  , resultResult :: Maybe r
  } deriving (Show,Functor)

instance FromJSON (GraphQLResult Value) where
  parseJSON = withObject "GraphQLResult" $ \o ->
    GraphQLResult
      <$> o .:? "errors" .!= []
      <*> o .:? "data"

-- | Get the errors in the @GraphQLResult@.
getErrors :: GraphQLResult r -> [GraphQLError]
getErrors = resultErrors

-- | Get the result of the @GraphQLResult@.
getResult :: GraphQLResult r -> Maybe r
getResult = resultResult

-- | A datatype to represent the schema of a GraphQL result.
data Schema where
  SchemaBool :: Schema
  SchemaInt :: Schema
  SchemaDouble :: Schema
  SchemaText :: Schema
  SchemaScalar :: Schema
  SchemaEnum :: (Typeable e, GraphQLEnum e) => Proxy e -> Schema
  SchemaMaybe :: Schema -> Schema
  SchemaList :: Schema -> Schema
  SchemaObject :: [(Text, Schema)] -> Schema

instance Show Schema where
  show = showSchema True
    where
      showSchema recurse = \case
        SchemaBool -> "SchemaBool"
        SchemaInt -> "SchemaInt"
        SchemaDouble -> "SchemaDouble"
        SchemaText -> "SchemaText"
        SchemaScalar -> "SchemaScalar"
        SchemaEnum _ -> "SchemaEnum"
        SchemaMaybe schema -> "SchemaMaybe " ++ showSchema False schema
        SchemaList schema -> if recurse
          then "SchemaList " ++ showSchema False schema
          else "SchemaList[..]"
        SchemaObject fields -> if recurse
          then "SchemaObject{" ++ intercalate ", " (map showField fields) ++ "}"
          else "SchemaObject{..}"
      showField (name, field) = Text.unpack name ++ "=" ++ showSchema False field

type GetterData = [(String, Schema)]

-- | Return a QuasiQuoter that can parse the given schema.
--
-- For example, with the given result and schema:
--
-- @
-- {
--   "foo": {              # SchemaObject
--      "a": 1,            #   SchemaInt
--      "nodes": [         #   SchemaList
--         { "b": true },  #     SchemaObject [("b", SchemaMaybe SchemaBool)]
--         { "b": false },
--         { "b": null },
--      ],
--      "c": "asdf",       #   SchemaText
--   }
-- }
-- @
--
-- If you have a variable named 'result' with the result of the GraphQL query,
-- the quasiquoter can be used in the following way:
--
-- @
-- get = getterFor schema -- the above schema
--
-- [get| result.foo.a |]          :: Int
-- [get| result.foo.nodes |]      :: [Object]
-- [get| result.foo.nodes[] |]    :: [Object]
-- [get| result.foo.nodes[].b |]  :: [Maybe Bool]
-- [get| result.foo.nodes[].b! |] :: [Bool]       -- will error at {"b": null}
-- [get| result.foo.c |]          :: Text
--
-- let nodes = [get| result.foo.nodes[] > node |]
--     bs = flip map nodes $ \node -> fromMaybe True [get| node.b |]
-- @
--
-- The QuasiQuoter follows the given rules:
--
-- * @x@ returns the value of @x@
--
-- * @x.y@ is only valid if @x@ is an @Object@ or a @Maybe Object@. Returns the value of the key @y@
--   in the Object(s).
--
-- * @x!@ is only valid if @x@ is a @Maybe@. Calls @fromJust@ on @x@.
--
-- * @x[]@ is only valid if @x@ is @[t]@. Applies remaining rules as an @fmap@ over the values of
--   @x@. Examples:
--
--     * @x[]@ without anything after is equivalent to @x@
--     * @x[].y@ gets the key @y@ in all the objects in @x@
--     * @x[]!@ calls @fromJust@ on all values in @x@
--
-- * @> name@ is only valid at the end of a getter for a value containing an @Object@. Stores the
--   schema of the contained @Object@ for subsequent queries. After storing the schema for @name@,
--   the schema can be used as normal: @[get| name.bar.etc |]@.
--
--     * @name@ needs to match the name of the value being queried. The following won't work:
--
--         @
--         let nodes = [get| result.foo.nodes[] > node |]
--             bs = flip map nodes $ \\n -> fromMaybe True [get| n.b |]
--                                                           -- ^ BAD: 'n' /= 'node'
--         @
--
--         That example will try to parse @n@ with the full @Result@ schema.
--
--     * @> name@ needs to run before using @name@. The following won't work:
--
--         @
--         do
--           result <- runQuery ...
--           let nodes = [get| result.foo.nodes[] > node |]
--           return $ map fromNode nodes
--         where
--           fromNode node = [get| node.b |]
--         @
--
--         That example won't know about @> node@ before compiling @fromNode@, so @node@ in @fromNode@
--         will be parsed with the full @Result@ schema as well.
getterFor :: Name -> Schema -> QuasiQuoter
getterFor resultCon fullSchema = QuasiQuoter
  { quoteExp = \s -> do
      GetterExpr{..} <- parse getterExpr s
      let innerResult = "result"
          varExp = varE $ mkName var
      result <- if var == innerResult
        then newName $ '_':innerResult -- prevent name clash between 'var' and (Result 'result')
        else newName innerResult

      getterData <- fromMaybe [] <$> getQ :: Q GetterData
      let varSchema = lookup var getterData
          initialSchema = fromMaybe fullSchema varSchema
          (getterFunc, finalSchema) = mkGetter terms initialSchema
          letDecl = if isNothing varSchema
            -- let (UnsafeResult result) = var
            then valD (conP resultCon [varP result]) (normalB varExp) []
            -- let result = Object var
            else valD (varP result) (normalB [| Object $varExp |]) []
          -- in ... $ result
          letExpr = appE getterFunc $ varE result

      case keep of
        Nothing -> return ()
        Just name -> putQ $
          -- undefined behavior if 'name' already exists in GetterData
          (name, getObjectSchema finalSchema) : getterData

      letE [letDecl] letExpr
  , quotePat = \_ -> error "'get' can only used as an expression"
  , quoteType = \_ -> error "'get' can only used as an expression"
  , quoteDec = \_ -> error "'get' can only used as an expression"
  }
  where
    parse p s = either (fail . parseErrorPretty) return $ runParser p s s
    mkGetter [] schema' = (getFinalizer schema', schema')
    mkGetter (term:terms) schema' = case term of
      GetterKey key -> case schema' of
        SchemaObject fields ->
          case lookup (Text.pack key) fields of
            Just field ->
              let (f, final) = mkGetter terms field
                  expr = case field of
                    SchemaMaybe _ ->[| $f . lookupKey $(lift key) |]
                    _ -> [| $f . getKey $(lift key) |]
              in (expr, final)
            Nothing -> error $
              "Invalid key: " ++ key ++ ". Possible keys: " ++
              intercalate ", " (map (Text.unpack . fst) fields)
        SchemaMaybe inner ->
          let (f, final) = mkGetter (term:terms) inner
              expr = [| ($f `mapMaybe`) |]
          in (expr, final)
        _ -> error $ "Cannot get key '" ++ key ++ "' at schema " ++ show schema'
      GetterBang -> case schema' of
        SchemaMaybe inner ->
          let (f, final) = mkGetter terms inner
              expr = [| $f . fromJust |]
          in (expr, final)
        _ -> error $ "Cannot use the '!' operator on the schema " ++ show schema'
      GetterList -> case schema' of
        SchemaList inner ->
          let (f, final) = mkGetter terms inner
              expr = [| ($f `mapList`) |]
          in (expr, final)
        _ -> error $ "Cannot use the '[]' operator on the schema " ++ show schema'
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
    getObjectSchema = \case
      SchemaMaybe inner -> getObjectSchema inner
      SchemaList inner -> getObjectSchema inner
      schema'@(SchemaObject _) -> schema'
      schema' -> error $ "Cannot store schema " ++ show schema'

{- Parser for getter quasiquotes -}

data GetterExpr = GetterExpr
  { var   :: String
  , terms :: [GetterTerm]
  , keep  :: Maybe String
  } deriving (Show)

data GetterTerm
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
  var <- identifier
  terms <- many getterTerm
  space
  keep <- (string ">" *> space *> fmap Just identifier) <|> pure Nothing
  space
  void eof
  return GetterExpr{..}

getterTerm :: Parser GetterTerm
getterTerm = parseGetterKey <|> parseGetterBang <|> parseGetterList
  where
    parseGetterKey = GetterKey <$> (string "." *> identifier)
    parseGetterBang = string "!" $> GetterBang
    parseGetterList = string "[]" $> GetterList
