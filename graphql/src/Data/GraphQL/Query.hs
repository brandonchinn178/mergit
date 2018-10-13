{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Query
  ( Query
  , HasArgs(..)
  , readGraphQLFile
  , getterFor
  , Schema(..)
  -- * Re-exports
  , Object
  , Proxy(..)
  , QuasiQuoter
  , object
  , (.=)
  ) where

import Data.Aeson (Object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.GraphQL.Query.Internal

-- | An alias for HashMap.fromList.
object :: [Pair] -> Object
object = HashMap.fromList

-- | A type class for GraphQL queries with arguments.
class HasArgs r where
  type QueryArgs r
  fromArgs :: Proxy r -> QueryArgs r -> Object

-- | A temporary function to read a graphql file and output it as a Query.
--
-- This function should go away when we generate the entire file with Template Haskell.
readGraphQLFile :: FilePath -> ExpQ
readGraphQLFile = undefined

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
-- The quasiquoter can be used in the following way:
--
-- @
-- get = getterFor schema -- the above schema
--
-- [get| foo.a |]         :: Result -> Int
-- [get| foo.nodes[] |]   :: Result -> [Object]
-- [get| foo.nodes[].b |] :: Result -> [Bool]
-- [get| foo.c |]         :: Result -> Maybe Text
-- [get| foo.c! |]        :: Result -> Text
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
  | forall e. SchemaEnum (Enum e => Proxy e)
  | SchemaMaybe Schema
  | SchemaList Schema
  | SchemaObject [(Text, Schema)]
