{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  GitHub.Schema.Event.CheckRun
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CheckRunEvent.
-}
module GitHub.Schema.Event.CheckRun where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Data.CheckRunStatus (CheckRunStatus)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.PullRequest (PullRequestShort)

mkEnum
  "CheckRunAction"
  [ "CREATED"
  , "REREQUESTED"
  , "REQUESTED_ACTION"
  , "COMPLETED"
  ]

mkEnum
  "CheckRunConclusion"
  [ "SUCCESS"
  , "FAILURE"
  , "NEUTRAL"
  , "CANCELLED"
  , "TIMED_OUT"
  , "ACTION_REQUIRED"
  ]

type CheckRunEvent =
  [schema|
  {
    action: CheckRunAction,
    check_run: {
      id: Int,
      head_sha: GitObjectID,
      status: CheckRunStatus,
      conclusion: Maybe CheckRunConclusion,
      name: Text,
      check_suite: {
        id: Int,
      },
      pull_requests: List #PullRequestShort,
    },
    requested_action: Maybe {
      identifier: Text,
    },
    #BaseEvent,
  }
|]
