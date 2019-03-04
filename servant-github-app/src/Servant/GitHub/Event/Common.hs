{-|
Module      :  Servant.GitHub.Event.Common
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines common types and schemas for GitHub events.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Common where

import Data.Aeson.Schema (schema)

{- Schemas used in every webhook: https://developer.github.com/webhooks/#payloads -}

type Sender = [schema|
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
    "owner": {
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
    },
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
    "sender": #Sender,
    "repository": Maybe #Repository,
    "organization": Maybe #Organization,
    "installation": Maybe #InstallationId,
  }
|]
