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
import Data.Aeson (FromJSON(..), Value, withObject, (.!=), (.:?))
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)
import Text.Megaparsec (Parsec, eof, parseErrorPretty, runParser)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space, string)
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

getErrors :: GraphQLResult r -> [GraphQLError]
getErrors = resultErrors

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
-- [get| result.foo.nodes[].b! |] :: [Bool]
-- [get| result.foo.c |]          :: Text
-- @
--
-- Rules:
--
-- * 'x' returns the value of 'x'
--
--     * Returns the value of 'x'
--     * Will be 'Maybe' if 'x' is within an inline fragment
--
-- * 'x.y' is only valid if 'x' is an 'Object' or a 'Maybe Object'. Returns the value of the key 'y'
--   in the Object(s).
--
-- * 'x!' is only valid if 'x' is a 'Maybe'. Calls 'fromJust' on 'x'.
--
-- * 'x[]' is only valid if 'x' is [t]. Applies remaining rules as an fmap over the values of 'x'.
--
--     * 'x[]' without anything after is equivalent to 'x'
--     * 'x[].y' gets the key 'y' in all the objects in 'x'
--     * 'x[]!' calls `fromJust` on all values in 'x'
getterFor :: Name -> Schema -> QuasiQuoter
getterFor resultCon schema = QuasiQuoter
  { quoteExp = \s -> do
      GetterExpr{..} <- parse getterExpr s
      let innerResult = "result"
      result <- if var == innerResult
        then newName $ '_':innerResult -- prevent name clash between 'var' and (Result 'result')
        else newName innerResult
      letE
        -- let (Result result) = var
        [valD (conP resultCon [varP result]) (normalB $ varE $ mkName var) []]
        -- in ... $ result
        $ appE (mkGetter terms schema) $ varE result
  , quotePat = \_ -> error "'get' can only used as an expression"
  , quoteType = \_ -> error "'get' can only used as an expression"
  , quoteDec = \_ -> error "'get' can only used as an expression"
  }
  where
    parse p s = either (fail . parseErrorPretty) return $ runParser p s s
    mkGetter [] schema' = case schema' of
      SchemaBool -> [| getBool |]
      SchemaInt -> [| getInt |]
      SchemaDouble -> [| getDouble |]
      SchemaText -> [| getText |]
      SchemaScalar -> [| getScalar |]
      SchemaEnum proxy ->
        let proxyType = typeRep proxy
        in [| getEnum $(proxyE $ typeRepToType proxyType) . getText |]
      SchemaMaybe inner -> [| mapMaybe $(mkGetter [] inner) |]
      SchemaList inner -> [| mapList $(mkGetter [] inner) |]
      SchemaObject _ -> [| getObject |]
    mkGetter (term:terms) schema' = case term of
      GetterKey key -> case schema' of
        SchemaObject fields ->
          case lookup (Text.pack key) fields of
            Just field -> case field of
              SchemaMaybe _ -> [| $(mkGetter terms field) . lookupKey $(lift key) |]
              _ -> [| $(mkGetter terms field) . getKey $(lift key) |]
            Nothing -> fail $
              "Invalid key: " ++ key ++ ". Possible keys: " ++
              intercalate ", " (map (Text.unpack . fst) fields)
        SchemaMaybe inner -> [| ($(mkGetter (term:terms) inner) `mapMaybe`) |]
        _ -> fail $ "Cannot get key '" ++ key ++ "' at schema " ++ show schema'
      GetterBang -> case schema' of
        SchemaMaybe inner -> [| $(mkGetter terms inner) . fromJust |]
        _ -> fail $ "Cannot use the '!' operator on the schema " ++ show schema'
      GetterList -> case schema' of
        SchemaList inner -> [| ($(mkGetter terms inner) `mapList`) |]
        _ -> fail $ "Cannot use the '[]' operator on the schema " ++ show schema'

{- Parser for getter quasiquotes -}

data GetterExpr = GetterExpr
  { var   :: String
  , terms :: [GetterTerm]
  } deriving (Show)

data GetterTerm
  = GetterKey String
  | GetterBang
  | GetterList
  deriving (Show)

type Parser = Parsec Void String

identifier :: Parser String
identifier = (:) <$> letterChar <*> many alphaNumChar

getterExpr :: Parser GetterExpr
getterExpr = do
  space
  var <- identifier
  terms <- many getterTerm
  space
  void eof
  return GetterExpr{..}

getterTerm :: Parser GetterTerm
getterTerm = parseGetterKey <|> parseGetterBang <|> parseGetterList
  where
    parseGetterKey = GetterKey <$> (string "." *> identifier)
    parseGetterBang = string "!" $> GetterBang
    parseGetterList = string "[]" $> GetterList
