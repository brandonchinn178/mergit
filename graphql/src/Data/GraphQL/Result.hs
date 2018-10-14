{-|
Module      :  Data.GraphQL.Result
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for defining schemas and querying GraphQL results.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.GraphQL.Result
  ( getterFor
  , Schema(..)
  , module Result
  -- * Re-exports
  , QuasiQuoter
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.GraphQL.Result.Parse as Result

-- | Return a QuasiQuoter that can parse the given schema.
--
-- For example, with the given result and schema:
--
-- @
-- {
--     "foo": {                  # SchemaObject
--          "a": 1,              #   SchemaInt
--          "nodes": [           #   SchemaList
--               { "b": true },  #     SchemaObject [("b", SchemaBool)]
--               { "b": false },
--               { "b": null },
--          ],
--          "c": "asdf",         #   SchemaMaybe SchemaString
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
-- [get| result.foo.a |]         :: Int
-- [get| result.foo.nodes[] |]   :: [Object]
-- [get| result.foo.nodes[].b |] :: [Bool]
-- [get| result.foo.c |]         :: Maybe Text
-- [get| result.foo.c! |]        :: Text
-- @
--
-- Rules:
--
-- * 'x' is only valid if 'x' is Object, Int, Double, Text, Bool, or Maybe t.
--
--     * Returns the value of 'x'
--     * 'Maybe t' if 'x' is within an inline fragment
--     * if 'x!' on 'Maybe t', calls 'fromJust'
--
-- * 'x[]' is only valid if 'x' is [t]. Returns the value of 'x'
-- * 'x.y' is only valid if 'x' is an Object, [Object], or Maybe Object. Returns the value of the
--   key 'y' in the Object(s).
getterFor :: Schema -> QuasiQuoter
getterFor _ = QuasiQuoter
  { quoteExp = undefined
  , quotePat = \_ -> error "'get' can only used as an expression"
  , quoteType = \_ -> error "'get' can only used as an expression"
  , quoteDec = \_ -> error "'get' can only used as an expression"
  }

-- | A datatype to represent the schema of a GraphQL result.
data Schema
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaString
  | SchemaScalar
  | forall e. SchemaEnum (GraphQLEnum e => Proxy e)
  | SchemaMaybe Schema
  | SchemaList Schema
  | SchemaObject [(Text, Schema)]
