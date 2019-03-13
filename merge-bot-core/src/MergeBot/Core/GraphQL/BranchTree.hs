{-|
Module      :  MergeBot.Core.GraphQL.BranchTree
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the BranchTree graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.BranchTree where

import Data.Aeson.Schema (schema)
import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.GraphQL.Custom.GitObjectID (GitObjectID)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _name      :: String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "name"      .= _name args
    ]

query :: Query
query = $(readGraphQLFile "BranchTree.graphql")

type Schema = [schema|
  {
    "repository": {
      "ref": Maybe {
        "target": {
          "tree": Maybe {
            "oid": GitObjectID,
          },
        },
      },
    },
  }
|]
