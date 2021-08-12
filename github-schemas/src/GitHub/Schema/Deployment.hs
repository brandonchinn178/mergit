{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      :  GitHub.Schema.Deployment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to deployments.
-}
module GitHub.Schema.Deployment where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)
import GitHub.Schema.User (UserShort)

type Deployment =
  [schema|
  {
    url: URL,
    id: Int,
    node_id: Text,
    sha: GitObjectID,
    ref: Text,
    task: Text,
    payload: {
      deploy: Text,
    },
    original_environment: Text,
    environment: Text,
    description: Maybe Text,
    creator: #UserShort,
    created_at: UTCTime,
    updated_at: UTCTime,
    statuses_url: URL,
    repository_url: URL,
    transient_environment: Bool,
    producation_environment: Bool,
  }
|]
