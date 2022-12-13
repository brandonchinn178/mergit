{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      :  Servant.GitHub.Event
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines all the possible GitHub events that can be sent.
-}
module Servant.GitHub.Event (
  GitHubEventType (..),
  IsGitHubEvent (..),
  module Schema,
) where

import Data.Aeson.Schema (Schema)
import Data.ByteString.Lazy (ByteString)
import GitHub.Schema.Event.CheckRun as Schema (CheckRunEvent)
import GitHub.Schema.Event.CheckSuite as Schema (CheckSuiteEvent)
import GitHub.Schema.Event.CommitComment as Schema (CommitCommentEvent)
import GitHub.Schema.Event.ContentReference as Schema (ContentReferenceEvent)
import GitHub.Schema.Event.Create as Schema (CreateEvent)
import GitHub.Schema.Event.Delete as Schema (DeleteEvent)
import GitHub.Schema.Event.Deployment as Schema (DeploymentEvent)
import GitHub.Schema.Event.DeploymentStatus as Schema (DeploymentStatusEvent)
import GitHub.Schema.Event.Fork as Schema (ForkEvent)
import GitHub.Schema.Event.GitHubAppAuthorization as Schema (
  GitHubAppAuthorizationEvent,
 )
import GitHub.Schema.Event.Installation as Schema (InstallationEvent)
import GitHub.Schema.Event.InstallationRepositories as Schema (
  InstallationRepositoriesEvent,
 )
import GitHub.Schema.Event.IssueComment as Schema (IssueCommentEvent)
import GitHub.Schema.Event.Issues as Schema (IssuesEvent)
import GitHub.Schema.Event.Label as Schema (LabelEvent)
import GitHub.Schema.Event.Ping as Schema (PingEvent)
import GitHub.Schema.Event.PullRequest as Schema (PullRequestEvent)
import GitHub.Schema.Event.Push as Schema (PushEvent)
import GitHub.Schema.Event.Status as Schema (StatusEvent)

{- ORMOLU_DISABLE -}
-- | TODO: Finish implementing other events.
data GitHubEventType
  = CheckRunEvent
  | CheckSuiteEvent
  | CommitCommentEvent
  | ContentReferenceEvent
  | CreateEvent
  | DeleteEvent
  | DeploymentEvent
  | DeploymentStatusEvent
  | ForkEvent
  | GitHubAppAuthorizationEvent
  | GollumEvent
  | InstallationEvent
  | InstallationRepositoriesEvent
  | IssueCommentEvent
  | IssuesEvent
  | LabelEvent
  -- TODO: MarketplacePurchaseEvent
  -- TODO: MemberEvent
  -- TODO: MembershipEvent
  -- TODO: MilestoneEvent
  -- TODO: OrganizationEvent
  -- TODO: OrgBlockEvent
  -- TODO: PageBuildEvent
  | PingEvent
  -- TODO: ProjectCardEvent
  -- TODO: ProjectColumnEvent
  -- TODO: ProjectEvent
  -- TODO: PublicEvent
  | PullRequestEvent
  -- TODO: PullRequestReviewEvent
  -- TODO: PullRequestReviewCommentEvent
  | PushEvent
  -- TODO: ReleaseEvent
  -- TODO: RepositoryEvent
  -- TODO: RepositoryImportEvent
  -- TODO: RepositoryVulnerabilityAlertEvent
  -- TODO: SecurityAdvisoryEvent
  | StatusEvent
  -- TODO: TeamEvent
  -- TODO: TeamAddEvent
  -- TODO: WatchEvent
  deriving (Show)
{- ORMOLU_ENABLE -}

class IsGitHubEvent (e :: GitHubEventType) where
  type EventSchema e :: Schema
  eventName :: ByteString

instance IsGitHubEvent 'CheckRunEvent where
  type EventSchema 'CheckRunEvent = Schema.CheckRunEvent
  eventName = "check_run"

instance IsGitHubEvent 'CheckSuiteEvent where
  type EventSchema 'CheckSuiteEvent = Schema.CheckSuiteEvent
  eventName = "check_suite"

instance IsGitHubEvent 'CommitCommentEvent where
  type EventSchema 'CommitCommentEvent = Schema.CommitCommentEvent
  eventName = "commit_comment"

instance IsGitHubEvent 'ContentReferenceEvent where
  type EventSchema 'ContentReferenceEvent = Schema.ContentReferenceEvent
  eventName = "content_reference"

instance IsGitHubEvent 'CreateEvent where
  type EventSchema 'CreateEvent = Schema.CreateEvent
  eventName = "create"

instance IsGitHubEvent 'DeleteEvent where
  type EventSchema 'DeleteEvent = Schema.DeleteEvent
  eventName = "delete"

instance IsGitHubEvent 'DeploymentEvent where
  type EventSchema 'DeploymentEvent = Schema.DeploymentEvent
  eventName = "deployment"

instance IsGitHubEvent 'DeploymentStatusEvent where
  type EventSchema 'DeploymentStatusEvent = Schema.DeploymentStatusEvent
  eventName = "deployment_status"

instance IsGitHubEvent 'ForkEvent where
  type EventSchema 'ForkEvent = Schema.ForkEvent
  eventName = "fork"

instance IsGitHubEvent 'GitHubAppAuthorizationEvent where
  type EventSchema 'GitHubAppAuthorizationEvent = Schema.GitHubAppAuthorizationEvent
  eventName = "github_app_authorization"

instance IsGitHubEvent 'InstallationEvent where
  type EventSchema 'InstallationEvent = Schema.InstallationEvent
  eventName = "installation"

instance IsGitHubEvent 'InstallationRepositoriesEvent where
  type EventSchema 'InstallationRepositoriesEvent = Schema.InstallationRepositoriesEvent
  eventName = "installation_repositories"

instance IsGitHubEvent 'IssueCommentEvent where
  type EventSchema 'IssueCommentEvent = Schema.IssueCommentEvent
  eventName = "issue_comment"

instance IsGitHubEvent 'IssuesEvent where
  type EventSchema 'IssuesEvent = Schema.IssuesEvent
  eventName = "issues"

instance IsGitHubEvent 'LabelEvent where
  type EventSchema 'LabelEvent = Schema.LabelEvent
  eventName = "label"

instance IsGitHubEvent 'PingEvent where
  type EventSchema 'PingEvent = Schema.PingEvent
  eventName = "ping"

instance IsGitHubEvent 'PullRequestEvent where
  type EventSchema 'PullRequestEvent = Schema.PullRequestEvent
  eventName = "pull_request"

instance IsGitHubEvent 'PushEvent where
  type EventSchema 'PushEvent = Schema.PushEvent
  eventName = "push"

instance IsGitHubEvent 'StatusEvent where
  type EventSchema 'StatusEvent = Schema.StatusEvent
  eventName = "status"
