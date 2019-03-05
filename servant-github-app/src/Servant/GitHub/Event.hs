{-|
Module      :  Servant.GitHub.Event
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines all the possible GitHub events that can be sent.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.GitHub.Event
  ( GitHubEventType(..)
  , IsGitHubEvent(..)
  , module EventSchemas
  ) where

import Data.Aeson.Schema (SchemaType)
import Data.ByteString.Lazy (ByteString)

import Servant.GitHub.Event.CheckRun as EventSchemas
import Servant.GitHub.Event.CheckSuite as EventSchemas
import Servant.GitHub.Event.CommitComment as EventSchemas
import Servant.GitHub.Event.ContentReference as EventSchemas
import Servant.GitHub.Event.Create as EventSchemas
import Servant.GitHub.Event.Delete as EventSchemas
import Servant.GitHub.Event.Deployment as EventSchemas
import Servant.GitHub.Event.DeploymentStatus as EventSchemas
import Servant.GitHub.Event.Fork as EventSchemas
import Servant.GitHub.Event.GitHubAppAuthorization as EventSchemas
import Servant.GitHub.Event.Installation as EventSchemas
import Servant.GitHub.Event.InstallationRepositories as EventSchemas
import Servant.GitHub.Event.IssueComment as EventSchemas
import Servant.GitHub.Event.Issues as EventSchemas
import Servant.GitHub.Event.Label as EventSchemas
import Servant.GitHub.Event.PullRequest as EventSchemas
import Servant.GitHub.Event.Push as EventSchemas

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
  -- | MarketplacePurchaseEvent
  -- | MemberEvent
  -- | MembershipEvent
  -- | MilestoneEvent
  -- | OrganizationEvent
  -- | OrgBlockEvent
  -- | PageBuildEvent
  -- | ProjectCardEvent
  -- | ProjectColumnEvent
  -- | ProjectEvent
  -- | PublicEvent
  | PullRequestEvent
  -- | PullRequestReviewEvent
  -- | PullRequestReviewCommentEvent
  | PushEvent
  -- | ReleaseEvent
  -- | RepositoryEvent
  -- | RepositoryImportEvent
  -- | RepositoryVulnerabilityAlertEvent
  -- | SecurityAdvisoryEvent
  -- | StatusEvent
  -- | TeamEvent
  -- | TeamAddEvent
  -- | WatchEvent
  deriving (Show)

class IsGitHubEvent (e :: GitHubEventType) where
  type EventSchema e :: SchemaType
  eventName :: ByteString

instance IsGitHubEvent 'CheckRunEvent where
  type EventSchema 'CheckRunEvent = CheckRunSchema
  eventName = "check_run"

instance IsGitHubEvent 'CheckSuiteEvent where
  type EventSchema 'CheckSuiteEvent = CheckSuiteSchema
  eventName = "check_suite"

instance IsGitHubEvent 'CommitCommentEvent where
  type EventSchema 'CommitCommentEvent = CommitCommentSchema
  eventName = "commit_comment"

instance IsGitHubEvent 'ContentReferenceEvent where
  type EventSchema 'ContentReferenceEvent = ContentReferenceSchema
  eventName = "content_reference"

instance IsGitHubEvent 'CreateEvent where
  type EventSchema 'CreateEvent = CreateSchema
  eventName = "create"

instance IsGitHubEvent 'DeleteEvent where
  type EventSchema 'DeleteEvent = DeleteSchema
  eventName = "delete"

instance IsGitHubEvent 'DeploymentEvent where
  type EventSchema 'DeploymentEvent = DeploymentSchema
  eventName = "deployment"

instance IsGitHubEvent 'DeploymentStatusEvent where
  type EventSchema 'DeploymentStatusEvent = DeploymentStatusSchema
  eventName = "deployment_status"

instance IsGitHubEvent 'ForkEvent where
  type EventSchema 'ForkEvent = ForkSchema
  eventName = "fork"

instance IsGitHubEvent 'GitHubAppAuthorizationEvent where
  type EventSchema 'GitHubAppAuthorizationEvent = GitHubAppAuthorizationSchema
  eventName = "github_app_authorization"

instance IsGitHubEvent 'InstallationEvent where
  type EventSchema 'InstallationEvent = InstallationSchema
  eventName = "installation"

instance IsGitHubEvent 'InstallationRepositoriesEvent where
  type EventSchema 'InstallationRepositoriesEvent = InstallationRepositoriesSchema
  eventName = "installation_repositories"

instance IsGitHubEvent 'IssueCommentEvent where
  type EventSchema 'IssueCommentEvent = IssueCommentSchema
  eventName = "issue_comment"

instance IsGitHubEvent 'IssuesEvent where
  type EventSchema 'IssuesEvent = IssuesSchema
  eventName = "issues"

instance IsGitHubEvent 'LabelEvent where
  type EventSchema 'LabelEvent = LabelSchema
  eventName = "label"

instance IsGitHubEvent 'PullRequestEvent where
  type EventSchema 'PullRequestEvent = PullRequestSchema
  eventName = "pull_request"

instance IsGitHubEvent 'PushEvent where
  type EventSchema 'PushEvent = PushSchema
  eventName = "push"
