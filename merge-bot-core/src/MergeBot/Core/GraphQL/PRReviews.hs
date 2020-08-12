{-|
Module      :  MergeBot.Core.GraphQL.PRReviews
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the PRReviews graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.PRReviews where

import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))
import GitHub.Data.PullRequestReviewState (PullRequestReviewState)

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _prNum     :: Int
  , _after     :: Maybe String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "prNum"     .= _prNum args
    , "after"     .= _after args
    ]

query :: Query
query = $(readGraphQLFile "PRReviews.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        reviews: Maybe {
          pageInfo: {
            hasNextPage: Bool,
            endCursor: Maybe Text,
          },
          nodes: Maybe List Maybe {
            author: Maybe {
              login: Text,
            },
            state: PullRequestReviewState,
          },
        },
      },
    },
  }
|]
