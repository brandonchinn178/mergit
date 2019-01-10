{-|
Module      :  Data.GraphQL.Query.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Query type and constructor.

Even though this module is exposed, the constructor should not be used in normal usage.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

module Data.GraphQL.Query.Internal (Query(..)) where

import Data.Text (Text)

import Data.GraphQL.Schema (SchemaType)

-- | A GraphQL Query that is validated at compile-time.
data Query (api :: k) (schema :: SchemaType) = UnsafeQuery Text
  deriving (Show)
