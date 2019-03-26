{-|
Module      :  MergeBot.Core.GraphQL.QueuedPRs
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the QueuedPRs graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.QueuedPRs where

import Data.Aeson.Schema (schema)
import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _after     :: Maybe String
  , _appId     :: Int
  , _checkName :: String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "after"     .= _after args
    , "appId"     .= _appId args
    , "checkName" .= _checkName args
    ]

query :: Query
query = $(readGraphQLFile "QueuedPRs.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      pullRequests: Maybe {
        pageInfo: {
          hasNextPage: Bool,
          endCursor: Maybe Text,
        },
        nodes: Maybe List Maybe {
          baseRefName: Text,
          headRef: Maybe {
            target: {
              checkSuites: Maybe {
                nodes: Maybe List Maybe {
                  checkRuns: Maybe {
                    nodes: Maybe List Maybe {
                      databaseId: Int,
                    },
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
