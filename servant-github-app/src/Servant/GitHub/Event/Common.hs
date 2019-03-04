{-|
Module      :  Servant.GitHub.Event.Common
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines common types and schemas for GitHub events.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Common where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text
import Data.Time (UTCTime)

{- Schemas used in every webhook: https://developer.github.com/webhooks/#payloads -}

type User = [schema|
  {
    "login": Text,
    "id": Int,
    "node_id": Text,
    "avatar_url": Text,
    "gravatar_id": Text,
    "url": Text,
    "html_url": Text,
    "followers_url": Text,
    "following_url": Text,
    "gists_url": Text,
    "starred_url": Text,
    "subscriptions_url": Text,
    "organizations_url": Text,
    "repos_url": Text,
    "events_url": Text,
    "received_events_url": Text,
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
    "html_url": Text,
    "description": Maybe Text,
    "fork": Bool,
    "url": Text,
    "forks_url": Text,
    "keys_url": Text,
    "collaborators_url": Text,
    "teams_url": Text,
    "hooks_url": Text,
    "issue_events_url": Text,
    "events_url": Text,
    "assignees_url": Text,
    "branches_url": Text,
    "tags_url": Text,
    "blobs_url": Text,
    "git_tags_url": Text,
    "git_refs_url": Text,
    "trees_url": Text,
    "statuses_url": Text,
    "languages_url": Text,
    "stargazers_url": Text,
    "contributors_url": Text,
    "subscribers_url": Text,
    "subscription_url": Text,
    "commits_url": Text,
    "git_commits_url": Text,
    "comments_url": Text,
    "issue_comment_url": Text,
    "contents_url": Text,
    "compare_url": Text,
    "merges_url": Text,
    "archive_url": Text,
    "downloads_url": Text,
    "issues_url": Text,
    "pulls_url": Text,
    "milestones_url": Text,
    "notifications_url": Text,
    "labels_url": Text,
    "releases_url": Text,
    "deployments_url": Text,
    "created_at": Text,
    "updated_at": Text,
    "pushed_at": Text,
    "git_url": Text,
    "ssh_url": Text,
    "clone_url": Text,
    "svn_url": Text,
    "homepage": Maybe Text,
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
    "mirror_url": Maybe Text,
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
    "url": Text,
    "repos_url": Text,
    "events_url": Text,
    "hooks_url": Text,
    "issues_url": Text,
    "members_url": Text,
    "public_members_url": Text,
    "avatar_url": Text,
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

{- Deployment -}

type Deployment = [schema|
  {
    "url": Text,
    "id": Int,
    "node_id": Text,
    "sha": Text,
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
    "statuses_url": Text,
    "repository_url": Text,
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
    "access_tokens_url": Text,
    "repositories_url": Text,
    "html_url": Text,
    "app_id": Int,
    "target_id": Int,
    "target_type": Text,
    "permissions": {
      "metadata": Text,
      "contents": Text,
      "issues": Text,
    },
    "events": List Text,
    "created_at": Int,
    "updated_at": Int,
    "single_file_name": Text,
  }
|]

{- Milestones -}

data MilestoneState = MilestoneOpen | MilestoneClosed
  deriving (Show)

instance FromJSON MilestoneState where
  parseJSON = withText "MilestoneState" $ \case
    "open" -> pure MilestoneOpen
    "closed" -> pure MilestoneClosed
    t -> fail $ "Bad MilestoneState: " ++ Text.unpack t

type Milestone = [schema|
  {
    "url": Text,
    "html_url": Text,
    "labels_url": Text,
    "id": Int,
    "node_id": Text,
    "number": Int,
    "state": MilestoneState,
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

{- Labels -}

type Label = [schema|
  {
    "id": Int,
    "node_id": Text,
    "url": Text,
    "name": Text,
    "description": Text,
    "color": Text,
    "default": Bool,
  }
|]

{- Pull requests -}

data PullRequestState = PullRequestOpen | PullRequestClosed
  deriving (Show)

instance FromJSON PullRequestState where
  parseJSON = withText "PullRequestState" $ \case
    "open" -> pure PullRequestOpen
    "closed" -> pure PullRequestClosed
    t -> fail $ "Bad PullRequestState: " ++ Text.unpack t

type PullRequest = [schema|
  {
    "url": Text,
    "id": Int,
    "node_id": Text,
    "html_url": Text,
    "diff_url": Text,
    "patch_url": Text,
    "issue_url": Text,
    "number": Int,
    "state": PullRequestState,
    "locked": Bool,
    "title": Text,
    "user": #User,
    "body": Text,
    "created_at": UTCTime,
    "updated_at": UTCTime,
    "closed_at": Maybe UTCTime,
    "merged_at": Maybe UTCTime,
    "merge_commit_sha": Text,
    "assignee": Maybe #User,
    "assignees": List #User,
    "requested_reviewers": List #User,
    "requested_teams": List #Team,
    "labels": List #Label,
    "milestone": Maybe #Milestone,
    "commits_url": Text,
    "review_comments_url": Text,
    "review_comment_url": Text,
    "comments_url": Text,
    "statuses_url": Text,
    "head": {
      "label": Text,
      "ref": Text,
      "sha": Text,
      "user": #User,
      "repo": #Repository,
    },
    "base": {
      "label": Text,
      "ref": Text,
      "sha": Text,
      "user": #User,
      "repo": #Repository
    },
    "_links": {
      "self": {
        "href": Text
      },
      "html": {
        "href": Text
      },
      "issue": {
        "href": Text
      },
      "comments": {
        "href": Text
      },
      "review_comments": {
        "href": Text
      },
      "review_comment": {
        "href": Text
      },
      "commits": {
        "href": Text
      },
      "statuses": {
        "href": Text
      }
    },
    "author_association": Text,
    "merged": Bool,
    "mergeable": Bool,
    "rebaseable": Bool,
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

{- Repository -}

type RepositoryShort = [schema|
  {
    "id": Int,
    "name": Text,
    "full_name": Text,
    "private": Bool,
  }
|]

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
    "url": Text,
    "name": Text,
    "slug": Text,
    "description": Text,
    "privacy": TeamPrivacy,
    "permission": Text,
    "members_url": Text,
    "repositories_url": Text,
    "parent": Maybe Int,
  }
|]
