{-|
Module      :  GitHub.Schema.Issue
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to issues.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Issue where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.State (State)
import GitHub.Data.URL (URL)
import GitHub.Schema.Label (Label)
import GitHub.Schema.Milestone (Milestone)
import GitHub.Schema.User (UserWebhook)

-- | https://developer.github.com/v3/issues/#get-a-single-issue
type Issue = [schema|
  {
    id: Int,
    node_id: Text,
    url: URL,
    repository_url: URL,
    labels_url: URL,
    comments_url: URL,
    events_url: URL,
    html_url: URL,
    number: Int,
    state: State,
    title: Text,
    body: Maybe Text,
    user: #UserWebhook,
    labels: List #Label,
    assignee: #UserWebhook,
    assignees: List #UserWebhook,
    milestone: Maybe #Milestone,
    locked: Bool,
    active_lock_reason: Maybe Text,
    comments: Int,
    pull_request: Maybe {
      url: URL,
      html_url: URL,
      diff_url: URL,
      patch_url: URL,
    },
    closed_at: Maybe UTCTime,
    created_at: Maybe UTCTime,
    updated_at: Maybe UTCTime,
    closed_by: #UserWebhook,
  }
|]
