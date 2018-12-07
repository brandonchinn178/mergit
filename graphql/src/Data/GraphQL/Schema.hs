{-|
Module      :  Data.GraphQL.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL response objects and the schema encoded in it.
-}

module Data.GraphQL.Schema
  ( Object(..)
  ) where

import qualified Data.Aeson as Aeson

newtype Object schema = UnsafeObject Aeson.Object
