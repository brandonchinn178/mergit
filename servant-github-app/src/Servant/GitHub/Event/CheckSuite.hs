{-|
Module      :  Servant.GitHub.Event.CheckSuite
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CheckSuiteEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.CheckSuite where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.CheckRun (CheckRunConclusion)
import Servant.GitHub.Event.Common

data CheckSuiteAction
  = CheckSuiteCompletedAction
  | CheckSuiteRequestedAction
  | CheckSuiteRerequestedAction
  deriving (Show)

instance FromJSON CheckSuiteAction where
  parseJSON = withText "CheckSuiteAction" $ \case
    "completed" -> pure CheckSuiteCompletedAction
    "requested" -> pure CheckSuiteRequestedAction
    "rerequested" -> pure CheckSuiteRerequestedAction
    t -> fail $ "Bad CheckSuiteAction: " ++ Text.unpack t

data CheckSuiteStatus
  = CheckSuiteRequested
  | CheckSuiteQueued
  | CheckSuiteInProgress
  | CheckSuiteCompleted
  deriving (Show)

instance FromJSON CheckSuiteStatus where
  parseJSON = withText "CheckSuiteStatus" $ \case
    "requested" -> pure CheckSuiteRequested
    "queued" -> pure CheckSuiteQueued
    "in_progress" -> pure CheckSuiteInProgress
    "completed" -> pure CheckSuiteCompleted
    t -> fail $ "Bad CheckSuiteStatus: " ++ Text.unpack t

type CheckSuiteSchema = [schema|
  {
    "action": CheckSuiteAction,
    "check_suite": {
      "head_branch": Text,
      "head_sha": SHA,
      "status": CheckSuiteStatus,
      "conclusion": Maybe CheckRunConclusion,
      "url": URL,
      "pull_requests": List #PullRequestShort,
    },
    #BaseEvent,
  }
|]
