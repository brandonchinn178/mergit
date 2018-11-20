{-|
Module      :  Data.GraphQL.Query.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the Query type and constructor.

Even though this module is exposed, the constructor should not be used in normal usage.
-}

module Data.GraphQL.Query.Internal (Query(..)) where

import Data.Text (Text)

-- | A GraphQL Query that is validated at compile-time.
data Query r = UnsafeQuery Text
  deriving (Show)
