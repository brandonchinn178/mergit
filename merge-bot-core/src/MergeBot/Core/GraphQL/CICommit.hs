{-|
Module      :  MergeBot.Core.GraphQL.CICommit
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the CICommit graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.CICommit where

import Data.Aeson.Schema (schema)
import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.StatusState (StatusState)

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _sha       :: GitObjectID
  , _after     :: Maybe String
  , _appId     :: Int
  , _checkName :: Maybe String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "sha"       .= _sha args
    , "after"     .= _after args
    , "appId"     .= _appId args
    , "checkName" .= _checkName args
    ]

query :: Query
query = $(readGraphQLFile "CICommit.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      object: Maybe {
        tree: Maybe {
          oid: GitObjectID,
          entries: Maybe List {
            name: Text,
            object: Maybe {
              text: Maybe Text,
            },
          },
        },
        status: Maybe {
          state: StatusState,
          contexts: List {
            context: Text,
            state: StatusState,
            targetUrl: Maybe Text,
          },
        },
        parents: Maybe {
          pageInfo: {
            hasNextPage: Bool,
            endCursor: Maybe Text,
          },
          nodes: Maybe List Maybe {
            oid: GitObjectID,
            checkSuites: Maybe {
              nodes: Maybe List Maybe {
                checkRuns: Maybe {
                  nodes: Maybe List Maybe {
                    databaseId: Int,
                    name: Text,
                  },
                },
              },
            },
          },
        },
      },
    },
  }
|]
