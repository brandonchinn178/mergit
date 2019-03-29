{-|
Module      :  MergeBot.Core.GraphQL.PRIsMerged
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PRIsMerged graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.PRIsMerged where

import Data.Aeson.Schema (schema)
import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _prNum     :: Int
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "prNum"     .= _prNum args
    ]

query :: Query
query = $(readGraphQLFile "PRIsMerged.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        merged: Bool,
      },
    },
  }
|]
