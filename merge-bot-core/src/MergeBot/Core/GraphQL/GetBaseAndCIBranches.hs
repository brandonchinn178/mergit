{-|
Module      :  MergeBot.Core.GraphQL.GetBaseAndCIBranches
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GetBaseAndCIBranches graphql query.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Core.GraphQL.GetBaseAndCIBranches where

import Data.Aeson.Schema (schema)
import Data.GraphQL hiding (Query)
import qualified Data.GraphQL as GraphQL
import Data.GraphQL.Aeson (object, (.=))
import GitHub.Data.GitObjectID (GitObjectID)

import MergeBot.Core.GraphQL.API (API)

type Query = GraphQL.Query API Args Schema

data Args = Args
  { _repoOwner  :: String
  , _repoName   :: String
  , _baseBranch :: String
  , _ciBranch   :: String
  } deriving (Show)

instance GraphQLArgs Args where
  fromArgs args = object
    [ "repoOwner"  .= _repoOwner args
    , "repoName"   .= _repoName args
    , "baseBranch" .= _baseBranch args
    , "ciBranch"   .= _ciBranch args
    ]

query :: Query
query = $(readGraphQLFile "GetBaseAndCIBranches.graphql")

type Schema = [schema|
  {
    repository: Maybe {
      baseBranch: Maybe {
        target: {
          oid: GitObjectID,
        },
      },
      ciBranch: Maybe {
        target: {
          oid: GitObjectID,
        },
      },
    },
  }
|]
