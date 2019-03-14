{-|
Module      :  GitHub.Schema.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to pull requests.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.PullRequest where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.State (State)
import GitHub.Data.URL (URL)
import GitHub.Schema.Commit (CommitShort)
import GitHub.Schema.Label (Label)
import GitHub.Schema.Milestone (Milestone)
import GitHub.Schema.Repository (RepoWebhook)
import GitHub.Schema.Team (Team)
import GitHub.Schema.User (UserWebhook)

-- | A pull request as returned by GitHub events.
type PullRequestWebhook = [schema|
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

-- | An abbreviated schema for PullRequest.
type PullRequestShort = [schema|
  {
    "url": URL,
    "id": Int,
    "number": Int,
    "head": #CommitShort,
    "base": #CommitShort,
  }
|]
