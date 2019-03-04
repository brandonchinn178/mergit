{-|
Module      :  Servant.GitHub.Event.CheckRun
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CheckRunEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.CheckRun where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

data CheckRunAction
  = CheckRunCreated
  | CheckRunRerequested
  | CheckRunRequestedAction
  deriving (Show)

instance FromJSON CheckRunAction where
  parseJSON = withText "CheckRunAction" $ \case
    "created" -> pure CheckRunCreated
    "rerequested" -> pure CheckRunRerequested
    "requested_action" -> pure CheckRunRequestedAction
    t -> fail $ "Bad CheckRunAction: " ++ Text.unpack t

data CheckRunStatus
  = CheckRunQueued
  | CheckRunInProgress
  | CheckRunCompleted
  deriving (Show)

instance FromJSON CheckRunStatus where
  parseJSON = withText "CheckRunStatus" $ \case
    "queued" -> pure CheckRunQueued
    "in_progress" -> pure CheckRunInProgress
    "completed" -> pure CheckRunCompleted
    t -> fail $ "Bad CheckRunStatus: " ++ Text.unpack t

data CheckRunConclusion
  = CheckRunSuccess
  | CheckRunFailure
  | CheckRunNeutral
  | CheckRunCancelled
  | CheckRunTimedOut
  | CheckRunActionRequired
  deriving (Show)

instance FromJSON CheckRunConclusion where
  parseJSON = withText "CheckRunConclusion" $ \case
    "success" -> pure CheckRunSuccess
    "failure" -> pure CheckRunFailure
    "neutral" -> pure CheckRunNeutral
    "cancelled" -> pure CheckRunCancelled
    "timed_out" -> pure CheckRunTimedOut
    "action_required" -> pure CheckRunActionRequired
    t -> fail $ "Bad CheckRunConclusion: " ++ Text.unpack t

type CheckRunSchema = [schema|
  {
    "action": CheckRunAction,
    "check_run": {
      "status": CheckRunStatus,
      "conclusion": Maybe CheckRunConclusion,
      "name": Text,
      "check_suite": {
        "id": Int,
      },
      "requested_action": {
        "identifier": Text,
      },
    },
    #BaseEvent,
  }
|]
