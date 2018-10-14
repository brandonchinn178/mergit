{-|
Module      :  Data.GraphQL.Error
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL Errors.
-}

module Data.GraphQL.Error
  ( GraphQLError(..)
  ) where

import Control.Exception (Exception)
import Data.Text (Text)

-- | An error that can be returned from the API.
newtype GraphQLError = GraphQLError { getError :: Text }
  deriving (Show)

instance Exception GraphQLError
