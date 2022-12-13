{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  GitHub.Schema.Event.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PullRequestEvent.
-}
module GitHub.Schema.Event.PullRequest where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.PullRequest (PullRequestWebhook)

-- https://docs.github.com/en/actions/reference/events-that-trigger-workflows#pull_request
mkEnum
  "PullRequestAction"
  [ "ASSIGNED"
  , "UNASSIGNED"
  , "LABELED"
  , "UNLABELED"
  , "OPENED"
  , "EDITED"
  , "CLOSED"
  , "REOPENED"
  , "SYNCHRONIZE"
  , "READY_FOR_REVIEW"
  , "LOCKED"
  , "UNLOCKED"
  , "REVIEW_REQUESTED"
  , "REVIEW_REQUEST_REMOVED"
  ]

type PullRequestEvent =
  [schema|
  {
    action: PullRequestAction,
    number: Int,
    changes: Maybe {
      title: Maybe {
        from: Text,
      },
      body: Maybe {
        from: Text,
      },
    },
    pull_request: #PullRequestWebhook,
    #BaseEvent,
  }
|]
