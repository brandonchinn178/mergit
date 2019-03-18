{-|
Module      :  MergeBot.Core.GraphQL.CommitTree
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the CommitTree graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.CommitTree where

import Data.Aeson.Schema (schema)
import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))
import GitHub.Data.GitObjectID (GitObjectID)

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _sha       :: GitObjectID
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "sha"      .= _sha args
    ]

query :: Query
query = $(readGraphQLFile "CommitTree.graphql")

type Schema = [schema|
  {
    "repository": {
      "object": Maybe {
        "tree": Maybe {
          "oid": GitObjectID,
          "entries": Maybe List {
            "name": Text,
            "object": Maybe {
              "text": Maybe Text,
            },
          },
        },
      },
    },
  }
|]
