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
{-# LANGUAGE TypeFamilies #-}

module Servant.GitHub.Event
  ( GitHubEventType(..)
  , IsGitHubEvent(..)
  , module EventSchemas
  ) where

import Data.Aeson.Schema (SchemaType)

import Servant.GitHub.Event.CheckRun as EventSchemas
import Servant.GitHub.Event.CheckSuite as EventSchemas
import Servant.GitHub.Event.CommitComment as EventSchemas
import Servant.GitHub.Event.ContentReference as EventSchemas

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

class IsGitHubEvent (e :: GitHubEventType) where
  type EventSchema e :: SchemaType
  eventName :: String

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
