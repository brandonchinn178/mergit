{-|
Module      :  MergeBot.Core.GraphQL.PRCheckRun
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PRCheckRun graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.PRCheckRun where

import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _prNum     :: Int
  , _appId     :: Int
  , _checkName :: String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "prNum"     .= _prNum args
    , "appId"     .= _appId args
    , "checkName" .= _checkName args
    ]

query :: Query
query = $(readGraphQLFile "PRCheckRun.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        commits: {
          nodes: Maybe List Maybe {
            commit: {
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
