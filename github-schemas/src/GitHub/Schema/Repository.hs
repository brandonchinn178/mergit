{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      :  GitHub.Schema.Repository
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to repositories.
-}
module GitHub.Schema.Repository where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.URL (URL)
import GitHub.Schema.User (UserShort)

-- | A repository returned by normal endpoints, e.g. `/repos/:owner/:repo`.
type Repository =
  [schema|
  {
    id: Int,
    node_id: Text,
    name: Text,
    full_name: Text,
    private: Bool,
    owner: #UserShort,
    html_url: Text,
    description: Maybe Text,
    fork: Bool,
    url: Text,
    forks_url: Text,
    keys_url: Text,
    collaborators_url: Text,
    teams_url: Text,
    hooks_url: Text,
    issue_events_url: Text,
    events_url: Text,
    assignees_url: Text,
    branches_url: Text,
    tags_url: Text,
    blobs_url: Text,
    git_tags_url: Text,
    git_refs_url: Text,
    trees_url: Text,
    statuses_url: Text,
    languages_url: Text,
    stargazers_url: Text,
    contributors_url: Text,
    subscribers_url: Text,
    subscription_url: Text,
    commits_url: Text,
    git_commits_url: Text,
    comments_url: Text,
    issue_comment_url: Text,
    contents_url: Text,
    compare_url: Text,
    merges_url: Text,
    archive_url: Text,
    downloads_url: Text,
    issues_url: Text,
    pulls_url: Text,
    milestones_url: Text,
    notifications_url: Text,
    labels_url: Text,
    releases_url: Text,
    deployments_url: Text,
    created_at: UTCTime,
    updated_at: UTCTime,
    pushed_at: UTCTime,
    git_url: Text,
    ssh_url: Text,
    clone_url: Text,
    svn_url: Text,
    homepage: Maybe Text,
    size: Int,
    stargazers_count: Int,
    watchers_count: Int,
    language: Maybe Text,
    has_issues: Bool,
    has_projects: Bool,
    has_downloads: Bool,
    has_wiki: Bool,
    has_pages: Bool,
    forks_count: Int,
    mirror_url: Maybe Text,
    archived: Bool,
    disabled: Bool,
    open_issues_count: Int,
    license: Maybe {
        key: Text,
        name: Text,
        spdx_id: Text,
        url: Maybe Text,
        node_id: Text,
    },
    forks: Int,
    open_issues: Int,
    watchers: Int,
    default_branch: Text,
    permissions: {
      admin: Bool,
      push: Bool,
      pull: Bool
    },
  }

|]

-- | A repository as returned by GitHub events.
type RepoWebhook =
  [schema|
  {
    id: Int,
    node_id: Text,
    name: Text,
    full_name: Text,
    owner: #UserShort,
    private: Bool,
    html_url: URL,
    description: Maybe Text,
    fork: Bool,
    url: URL,
    forks_url: URL,
    keys_url: URL,
    collaborators_url: URL,
    teams_url: URL,
    hooks_url: URL,
    issue_events_url: URL,
    events_url: URL,
    assignees_url: URL,
    branches_url: URL,
    tags_url: URL,
    blobs_url: URL,
    git_tags_url: URL,
    git_refs_url: URL,
    trees_url: URL,
    statuses_url: URL,
    languages_url: URL,
    stargazers_url: URL,
    contributors_url: URL,
    subscribers_url: URL,
    subscription_url: URL,
    commits_url: URL,
    git_commits_url: URL,
    comments_url: URL,
    issue_comment_url: URL,
    contents_url: URL,
    compare_url: URL,
    merges_url: URL,
    archive_url: URL,
    downloads_url: URL,
    issues_url: URL,
    pulls_url: URL,
    milestones_url: URL,
    notifications_url: URL,
    labels_url: URL,
    releases_url: URL,
    deployments_url: URL,
    // https://github.com/isaacs/github/issues/1534
    // created_at: UTCTime,
    // updated_at: UTCTime,
    // pushed_at: UTCTime,
    git_url: URL,
    ssh_url: URL,
    clone_url: URL,
    svn_url: URL,
    homepage: Maybe URL,
    size: Int,
    stargazers_count: Int,
    watchers_count: Int,
    language: Maybe Text,
    has_issues: Bool,
    has_projects: Bool,
    has_downloads: Bool,
    has_wiki: Bool,
    has_pages: Bool,
    forks_count: Int,
    mirror_url: Maybe URL,
    archived: Bool,
    open_issues_count: Int,
    license: Maybe {
        key: Text,
        name: Text,
        spdx_id: Text,
        url: Maybe Text,
        node_id: Text,
    },
    forks: Int,
    open_issues: Int,
    watchers: Int,
    default_branch: Text,
  }
|]

-- | An abbreviated schema for Repository.
type RepositoryShort =
  [schema|
  {
    id: Int,
    name: Text,
    full_name: Text,
    private: Bool,
  }
|]
