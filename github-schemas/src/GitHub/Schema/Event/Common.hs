{-|
Module      :  GitHub.Schema.Event.Common
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines common types and schemas for GitHub events.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Common where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text
import Data.Time (UTCTime)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)
import GitHub.Schema.Commit (CommitShort)
import GitHub.Schema.Repository (RepoWebhook)
import GitHub.Schema.User (UserWebhook)

{- Issue: https://developer.github.com/v3/issues/#get-a-single-issue -}

type Issue = [schema|
  {
    "id": Int,
    "node_id": Text,
    "url": URL,
    "repository_url": URL,
    "labels_url": URL,
    "comments_url": URL,
    "events_url": URL,
    "html_url": URL,
    "number": Int,
    "state": State,
    "title": Text,
    "body": Maybe Text,
    "user": #UserWebhook,
    "labels": List #Label,
    "assignee": #UserWebhook,
    "assignees": List #UserWebhook,
    "milestone": Maybe #Milestone,
    "locked": Bool,
    "active_lock_reason": Maybe Text,
    "comments": Int,
    "pull_request": Maybe {
      "url": URL,
      "html_url": URL,
      "diff_url": URL,
      "patch_url": URL,
    },
    "closed_at": Maybe UTCTime,
    "created_at": Maybe UTCTime,
    "updated_at": Maybe UTCTime,
    "closed_by": #UserWebhook,
  }
|]

{- Labels: https://developer.github.com/v3/issues/labels/#get-a-single-label -}

type Label = [schema|
  {
    "id": Int,
    "node_id": Text,
    "url": URL,
    "name": Text,
    "description": Text,
    "color": Text,
    "default": Bool,
  }
|]

{- Milestones -}

type Milestone = [schema|
  {
    "url": URL,
    "html_url": URL,
    "labels_url": URL,
    "id": Int,
    "node_id": Text,
    "number": Int,
    "state": State,
    "title": Text,
    "description": Text,
    "creator": #UserWebhook,
    "open_issues": Int,
    "closed_issues": Int,
    "created_at": UTCTime,
    "updated_at": UTCTime,
    "closed_at": Maybe UTCTime,
    "due_on": UTCTime,
  }
|]

{- Pull requests -}

type PullRequest = [schema|
  {
    "url": URL,
    "id": Int,
    "node_id": Text,
    "html_url": URL,
    "diff_url": URL,
    "patch_url": URL,
    "issue_url": URL,
    "number": Int,
    "state": State,
    "locked": Bool,
    "title": Text,
    "user": #UserWebhook,
    "body": Text,
    "created_at": UTCTime,
    "updated_at": UTCTime,
    "closed_at": Maybe UTCTime,
    "merged_at": Maybe UTCTime,
    "merge_commit_sha": Maybe GitObjectID,
    "assignee": Maybe #UserWebhook,
    "assignees": List #UserWebhook,
    "requested_reviewers": List #UserWebhook,
    "requested_teams": List #Team,
    "labels": List #Label,
    "milestone": Maybe #Milestone,
    "commits_url": URL,
    "review_comments_url": URL,
    "review_comment_url": URL,
    "comments_url": URL,
    "statuses_url": URL,
    "head": {
      "label": Text,
      "ref": Text,
      "sha": GitObjectID,
      "user": #UserWebhook,
      "repo": #RepoWebhook,
    },
    "base": {
      "label": Text,
      "ref": Text,
      "sha": GitObjectID,
      "user": #UserWebhook,
      "repo": #RepoWebhook,
    },
    "_links": {
      "self": {
        "href": URL
      },
      "html": {
        "href": URL
      },
      "issue": {
        "href": URL
      },
      "comments": {
        "href": URL
      },
      "review_comments": {
        "href": URL
      },
      "review_comment": {
        "href": URL
      },
      "commits": {
        "href": URL
      },
      "statuses": {
        "href": URL
      },
    },
    "author_association": Text,
    "merged": Bool,
    "mergeable": Maybe Bool,
    "rebaseable": Maybe Bool,
    "mergeable_state": Text,
    "merged_by": Maybe #UserWebhook,
    "comments": Int,
    "review_comments": Int,
    "maintainer_can_modify": Bool,
    "commits": Int,
    "additions": Int,
    "deletions": Int,
    "changed_files": Int
  }
|]

type PullRequestShort = [schema|
  {
    "url": URL,
    "id": Int,
    "number": Int,
    "head": #CommitShort,
    "base": #CommitShort,
  }
|]

{- Repository -}

type RepositoryShort = [schema|
  {
    "id": Int,
    "name": Text,
    "full_name": Text,
    "private": Bool,
  }
|]

{- State -}

data State = StateOpen | StateClosed
  deriving (Show)

instance FromJSON State where
  parseJSON = withText "State" $ \case
    "open" -> pure StateOpen
    "closed" -> pure StateClosed
    t -> fail $ "Bad State: " ++ Text.unpack t

{- Teams -}

data TeamPrivacy = TeamSecret | TeamClosed
  deriving (Show)

instance FromJSON TeamPrivacy where
  parseJSON = withText "TeamPrivacy" $ \case
    "secret" -> pure TeamSecret
    "closed" -> pure TeamClosed
    t -> fail $ "Bad TeamPrivacy: " ++ Text.unpack t

type Team = [schema|
  {
    "id": Int,
    "node_id": Text,
    "url": URL,
    "name": Text,
    "slug": Text,
    "description": Text,
    "privacy": TeamPrivacy,
    "permission": Text,
    "members_url": URL,
    "repositories_url": URL,
    "parent": Maybe Int,
  }
|]
