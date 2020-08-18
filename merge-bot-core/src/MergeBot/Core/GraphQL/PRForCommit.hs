{-|
Module      :  MergeBot.Core.GraphQL.PRForCommit
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PRForCommit graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.PRForCommit where

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
  , _after     :: Maybe String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "sha"       .= _sha args
    , "after"     .= _after args
    ]

query :: Query
query = $(readGraphQLFile "PRForCommit.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      object: Maybe {
        associatedPullRequests: Maybe {
          pageInfo: {
            hasNextPage: Bool,
            endCursor: Maybe Text,
          },
          nodes: Maybe List Maybe {
            headRefOid: GitObjectID,
            number: Int,
            headRef: Maybe {
              name: Text,
            },
            merged: Bool,
          },
        },
      },
    },
  }
|]
