{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      :  GitHub.Schema.Milestone
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to milestones.
-}
module GitHub.Schema.Milestone where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.State (State)
import GitHub.Data.URL (URL)
import GitHub.Schema.User (UserShort)

-- | https://developer.github.com/v3/issues/milestones/#get-a-single-milestone
type Milestone =
  [schema|
  {
    url: URL,
    html_url: URL,
    labels_url: URL,
    id: Int,
    node_id: Text,
    number: Int,
    state: State,
    title: Text,
    description: Maybe Text,
    creator: #UserShort,
    open_issues: Int,
    closed_issues: Int,
    created_at: UTCTime,
    updated_at: UTCTime,
    closed_at: Maybe UTCTime,
    due_on: Maybe UTCTime,
  }
|]
