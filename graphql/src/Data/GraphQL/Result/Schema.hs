{-|
Module      :  Data.GraphQL.Result.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for defining schemas for GraphQL results.

GraphQL guarantees that the result of a successful query will be in a pre-determined format. We can
define this schema at compile time and be able to parse a result with type-checked operations.

Take the following GraphQL query as an example:

@
query Foo {
  foo {
    a
    nodes {
      b
    }
    c
  }
}
@

The GraphQL API being sent this query knows the types of each field, so we can use those types to
generate the following schema for the above:

@
schema :: Schema
schema = SchemaObject
  [ ("foo", SchemaObject
      [ ("a", SchemaInt)
      , ("nodes", SchemaList $ SchemaObject
          [ ("b", SchemaMaybe SchemaBool)
          ]
        )
      , ("c", SchemaText)
      ]
    )
  ]
@

An example result for this query might be (excluding the top-level "data" field):

@
{
  "foo": {
     "a": 1,
     "nodes": [
        { "b": true },
        { "b": false },
        { "b": null }
     ],
     "c": "asdf"
  }
}
@
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GraphQL.Result.Schema
  ( Schema(..)
  , GraphQLEnum(..)
  , fromText
  -- * Re-exports
  , Proxy(..)
  ) where

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, typeRep)

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

instance Eq Schema where
  SchemaBool == SchemaBool = True
  SchemaInt == SchemaInt = True
  SchemaDouble == SchemaDouble = True
  SchemaText == SchemaText = True
  SchemaScalar == SchemaScalar = True
  SchemaEnum p0 == SchemaEnum p1 = typeRep p0 == typeRep p1
  SchemaMaybe s0 == SchemaMaybe s1 = s0 == s1
  SchemaList s0 == SchemaList s1 = s0 == s1
  SchemaObject f1 == SchemaObject f2 = f1 == f2
  _ == _ = False

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

-- | A type class for parsing 'SchemaEnum'.
class GraphQLEnum e where
  getEnum :: Proxy e -> Text -> e

-- | An alias for 'Text.unpack'.
fromText :: Text -> String
fromText = Text.unpack
