{-|
Module      :  Servant.GitHub.Event.Common
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines common types and schemas for GitHub events.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Common where

import Data.Aeson (FromJSON(..), ToJSON, withText)
import Data.Aeson.Schema (schema)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

{- Basic data types -}

newtype SHA = SHA { unSHA :: Text }
  deriving (Show,FromJSON,ToJSON)

type URL = Text

{- Schemas used in every webhook: https://developer.github.com/webhooks/#payloads -}

type User = [schema|
  {
    "login": Text,
    "id": Int,
    "node_id": Text,
    "avatar_url": URL,
    "gravatar_id": Text,
    "url": Text,
    "html_url": URL,
    "followers_url": URL,
    "following_url": URL,
    "gists_url": URL,
    "starred_url": URL,
    "subscriptions_url": URL,
    "organizations_url": URL,
    "repos_url": URL,
    "events_url": URL,
    "received_events_url": URL,
    "type": Text,
    "site_admin": Bool,
  }
|]

type Repository = [schema|
  {
    "id": Int,
    "node_id": Text,
    "name": Text,
    "full_name": Text,
    "owner": #User,
    "private": Bool,
    "html_url": URL,
    "description": Maybe Text,
    "fork": Bool,
    "url": URL,
    "forks_url": URL,
    "keys_url": URL,
    "collaborators_url": URL,
    "teams_url": URL,
    "hooks_url": URL,
    "issue_events_url": URL,
    "events_url": URL,
    "assignees_url": URL,
    "branches_url": URL,
    "tags_url": URL,
    "blobs_url": URL,
    "git_tags_url": URL,
    "git_refs_url": URL,
    "trees_url": URL,
    "statuses_url": URL,
    "languages_url": URL,
    "stargazers_url": URL,
    "contributors_url": URL,
    "subscribers_url": URL,
    "subscription_url": URL,
    "commits_url": URL,
    "git_commits_url": URL,
    "comments_url": URL,
    "issue_comment_url": URL,
    "contents_url": URL,
    "compare_url": URL,
    "merges_url": URL,
    "archive_url": URL,
    "downloads_url": URL,
    "issues_url": URL,
    "pulls_url": URL,
    "milestones_url": URL,
    "notifications_url": URL,
    "labels_url": URL,
    "releases_url": URL,
    "deployments_url": URL,
    "created_at": UTCTime,
    "updated_at": UTCTime,
    "pushed_at": UTCTime,
    "git_url": URL,
    "ssh_url": URL,
    "clone_url": URL,
    "svn_url": URL,
    "homepage": Maybe URL,
    "size": Int,
    "stargazers_count": Int,
    "watchers_count": Int,
    "language": Maybe Text,
    "has_issues": Bool,
    "has_projects": Bool,
    "has_downloads": Bool,
    "has_wiki": Bool,
    "has_pages": Bool,
    "forks_count": Int,
    "mirror_url": Maybe URL,
    "archived": Bool,
    "open_issues_count": Int,
    "license": Maybe Text,
    "forks": Int,
    "open_issues": Int,
    "watchers": Int,
    "default_branch": Text,
  }
|]

type Organization = [schema|
  {
    "login": Text,
    "id": Int,
    "node_id": Text,
    "url": URL,
    "repos_url": URL,
    "events_url": URL,
    "hooks_url": URL,
    "issues_url": URL,
    "members_url": URL,
    "public_members_url": URL,
    "avatar_url": URL,
    "description": Text,
  }
|]

type InstallationId = [schema|
  {
    "id": Int
  }
|]

type BaseEvent = [schema|
  {
    "sender": #User,
    "repository": Maybe #Repository,
    "organization": Maybe #Organization,
    "installation": Maybe #InstallationId,
  }
|]

{- Comment: https://developer.github.com/v3/issues/comments/#get-a-single-comment -}

type Comment = [schema|
  {
    "id": Int,
    "node_id": Text,
    "url": Text,
    "html_url": URL,
    "body": Text,
    "user": #User,
    "created_at": UTCTime,
    "updated_at": UTCTime,
  }
|]

{- Commit -}

type CommitShort = [schema|
  {
    "ref": Text,
    "sha": SHA,
    "repo": {
      "id": Int,
      "url": URL,
      "name": Text,
    },
  }
|]

{- Deployment -}

type Deployment = [schema|
  {
    "url": URL,
    "id": Int,
    "node_id": Text,
    "sha": SHA,
    "ref": Text,
    "task": Text,
    "payload": {
      "deploy": Text,
    },
    "original_environment": Text,
    "environment": Text,
    "description": Maybe Text,
    "creator": #User,
    "created_at": UTCTime,
    "updated_at": UTCTime,
    "statuses_url": URL,
    "repository_url": URL,
    "transient_environment": Bool,
    "producation_environment": Bool,
  }
|]

{- Installation -}

type Installation = [schema|
  {
    "id": Int,
    "account": #User,
    "repository_selection": Text,
    "access_tokens_url": URL,
    "repositories_url": URL,
    "html_url": URL,
    "app_id": Int,
    "target_id": Int,
    "target_type": Text,
    "permissions": {
      "checks": Text,
      "metadata": Text,
    },
    "events": List Text,
    "created_at": Int,
    "updated_at": Int,
    "single_file_name": Maybe Text,
  }
|]

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
    "user": #User,
    "labels": List #Label,
    "assignee": #User,
    "assignees": List #User,
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
    "closed_by": #User,
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
    "creator": #User,
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
    "user": #User,
    "body": Text,
    "created_at": UTCTime,
    "updated_at": UTCTime,
    "closed_at": Maybe UTCTime,
    "merged_at": Maybe UTCTime,
    "merge_commit_sha": Maybe SHA,
    "assignee": Maybe #User,
    "assignees": List #User,
    "requested_reviewers": List #User,
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
      "sha": SHA,
      "user": #User,
      "repo": #Repository,
    },
    "base": {
      "label": Text,
      "ref": Text,
      "sha": SHA,
      "user": #User,
      "repo": #Repository
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
    "merged_by": Maybe #User,
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
