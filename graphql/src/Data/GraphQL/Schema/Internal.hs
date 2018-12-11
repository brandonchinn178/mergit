{-|
Module      :  Data.GraphQL.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL response objects and the schema encoded in it.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

module Data.GraphQL.Schema.Internal where

import qualified Data.Aeson as Aeson
import GHC.TypeLits (Symbol)

-- | The object returned by a GraphQL query, containing the schema of the JSON object.
newtype Object (schema :: SchemaType) = UnsafeObject Aeson.Object
  deriving (Show)

{- Schema definition -}

-- | The schema definition for a GraphQL result.
data SchemaGraph s
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaScalar s
  | SchemaEnum s
  | SchemaMaybe (SchemaGraph s)
  | SchemaList (SchemaGraph s)
  | SchemaObject [(s, SchemaGraph s)]
  deriving (Show)

-- | A 'SchemaGraph' at the kind level.
type SchemaType = SchemaGraph Symbol
