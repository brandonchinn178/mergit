{-|
Module      :  Data.GraphQL.Result
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for defining schemas and querying GraphQL results.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.GraphQL.Result
  ( Schema(..)
  , getterFor
  , module Result
  -- * Re-exports
  , QuasiQuoter
  ) where

import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Void (Void)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Megaparsec (Parsec, eof, parse, parseErrorPretty)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space, string)

import Data.GraphQL.Result.Parse as Result

-- | A datatype to represent the schema of a GraphQL result.
data Schema
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaScalar
  | forall e. SchemaEnum (GraphQLEnum e => Proxy e)
  | SchemaMaybe Schema
  | SchemaList Schema
  | SchemaObject [(Text, Schema)]

-- | Return a QuasiQuoter that can parse the given schema.
--
-- For example, with the given result and schema:
--
-- @
-- {
--     "foo": {                  # SchemaObject
--          "a": 1,              #   SchemaInt
--          "nodes": [           #   SchemaList
--               { "b": true },  #     SchemaObject [("b", SchemaMaybe SchemaBool)]
--               { "b": false },
--               { "b": null },
--          ],
--          "c": "asdf",         #   SchemaText
--     }
-- }
-- @
--
-- If you have a variable named 'result' with the result of the GraphQL query, the quasiquoter can
-- be used in the following way:
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
getterFor :: Schema -> QuasiQuoter
getterFor _ = QuasiQuoter
  { quoteExp = \s -> do
      expr <- either (fail . parseErrorPretty) return $ parse getterExpr s s
      error "TODO"
  , quotePat = \_ -> error "'get' can only used as an expression"
  , quoteType = \_ -> error "'get' can only used as an expression"
  , quoteDec = \_ -> error "'get' can only used as an expression"
  }

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
    parseGetterBang = string "!" *> pure GetterBang
    parseGetterList = string "[]" *> pure GetterList
