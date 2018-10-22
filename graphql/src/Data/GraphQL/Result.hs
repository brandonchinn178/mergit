{-|
Module      :  Data.GraphQL.Result
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for defining schemas and querying GraphQL results.
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.GraphQL.Result
  ( GraphQLResult
  , getErrors
  , getResult
  , module Result
  -- * Re-exports
  , Value
  ) where

import Data.Aeson (FromJSON(..), Value, withObject, (.!=), (.:?))

import Data.GraphQL.Error (GraphQLError)
import Data.GraphQL.Result.Aeson as Result
import Data.GraphQL.Result.Getter as Result
import Data.GraphQL.Result.Schema as Result

-- | A result of a GraphQL query.
data GraphQLResult r = GraphQLResult
  { resultErrors :: [GraphQLError]
  , resultResult :: Maybe r
  } deriving (Show,Functor)

instance FromJSON (GraphQLResult Value) where
  parseJSON = withObject "GraphQLResult" $ \o ->
    GraphQLResult
      <$> o .:? "errors" .!= []
      <*> o .:? "data"

-- | Get the errors in the @GraphQLResult@.
getErrors :: GraphQLResult r -> [GraphQLError]
getErrors = resultErrors

-- | Get the result of the @GraphQLResult@.
getResult :: GraphQLResult r -> Maybe r
getResult = resultResult
