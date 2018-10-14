{-|
Module      :  Data.GraphQL.Result.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Helpers for parsing GraphQL results.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.GraphQL.Result.Parse
  ( GraphQLEnum(..)
  , fromText
  -- * Re-exports
  , Proxy(..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | An alias for Text.unpack.
fromText :: Text -> String
fromText = Text.unpack

-- | A type class for parsing SchemaEnum.
class GraphQLEnum e where
  getEnum :: Proxy e -> Text -> e
