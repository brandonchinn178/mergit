{-|
Module      :  GitHub.Schema.Event.CheckSuite
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CheckSuiteEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.CheckSuite where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Data.CheckSuiteStatus (CheckSuiteStatus)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)
import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Event.CheckRun (CheckRunConclusion)
import GitHub.Schema.PullRequest (PullRequestShort)

mkEnum "CheckSuiteAction"
  [ "COMPLETED"
  , "REQUESTED"
  , "REREQUESTED"
  ]

type CheckSuiteEvent = [schema|
  {
    "action": CheckSuiteAction,
    "check_suite": {
      "head_branch": Text,
      "head_sha": GitObjectID,
      "status": CheckSuiteStatus,
      "conclusion": Maybe CheckRunConclusion,
      "url": URL,
      "pull_requests": List #PullRequestShort,
    },
    #BaseEvent,
  }
|]
