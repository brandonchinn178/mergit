{-|
Module      :  Servant.GitHub.Event
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines all the possible GitHub events that can be sent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.GitHub.Event
  ( GitHubEventType(..)
  , FromEventType
  , eventName
  ) where

data GitHubEventType
  = CheckRunEvent
  | CheckSuiteEvent
  | CommitCommentEvent
  | ContentReferenceEvent
  | CreateEvent
  | DeleteEvent
  | DeploymentEvent
  | DeploymentStatusEvent
  | DownloadEvent
  | FollowEvent
  | ForkEvent
  | ForkApplyEvent
  | GitHubAppAuthorizationEvent
  | GistEvent
  | GollumEvent
  | InstallationEvent
  | InstallationRepositoriesEvent
  | IssueCommentEvent
  | IssuesEvent
  | LabelEvent
  | MarketplacePurchaseEvent
  | MemberEvent
  | MembershipEvent
  | MilestoneEvent
  | OrganizationEvent
  | OrgBlockEvent
  | PageBuildEvent
  | ProjectCardEvent
  | ProjectColumnEvent
  | ProjectEvent
  | PublicEvent
  | PullRequestEvent
  | PullRequestReviewEvent
  | PullRequestReviewCommentEvent
  | PushEvent
  | ReleaseEvent
  | RepositoryEvent
  | RepositoryImportEvent
  | RepositoryVulnerabilityAlertEvent
  | SecurityAdvisoryEvent
  | StatusEvent
  | TeamEvent
  | TeamAddEvent
  | WatchEvent
  deriving (Show)

eventName :: GitHubEventType -> String
eventName = \case
  CheckRunEvent -> "check_run"
  CheckSuiteEvent -> "check_suite"
  CommitCommentEvent -> "commit_comment"
  ContentReferenceEvent -> "content_reference"
  CreateEvent -> "create"
  DeleteEvent -> "delete"
  DeploymentEvent -> "deployment"
  DeploymentStatusEvent -> "deployment_status"
  DownloadEvent -> "download"
  FollowEvent -> "follow"
  ForkEvent -> "fork"
  ForkApplyEvent -> "fork_apply"
  GitHubAppAuthorizationEvent -> "github_app_authorization"
  GistEvent -> "gist"
  GollumEvent -> "gollum"
  InstallationEvent -> "installation"
  InstallationRepositoriesEvent -> "installation_repositories"
  IssueCommentEvent -> "issue_comment"
  IssuesEvent -> "issues"
  LabelEvent -> "label"
  MarketplacePurchaseEvent -> "marketplace_purchase"
  MemberEvent -> "member"
  MembershipEvent -> "membership"
  MilestoneEvent -> "milestone"
  OrganizationEvent -> "organization"
  OrgBlockEvent -> "org_block"
  PageBuildEvent -> "page_build"
  ProjectCardEvent -> "project_card"
  ProjectColumnEvent -> "project_column"
  ProjectEvent -> "project"
  PublicEvent -> "public"
  PullRequestEvent -> "pull_request"
  PullRequestReviewEvent -> "pull_request_review"
  PullRequestReviewCommentEvent -> "pull_request_review_comment"
  PushEvent -> "push"
  ReleaseEvent -> "release"
  RepositoryEvent -> "repository"
  RepositoryImportEvent -> "repository_import"
  RepositoryVulnerabilityAlertEvent -> "repository_vulnerability_alert"
  SecurityAdvisoryEvent -> "security_advisory"
  StatusEvent -> "status"
  TeamEvent -> "team"
  TeamAddEvent -> "team_add"
  WatchEvent -> "watch"

type family FromEventType (e :: GitHubEventType) :: *
