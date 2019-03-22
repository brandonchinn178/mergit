{-|
Module      :  GitHub.Schema.Event.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PullRequestEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.PullRequest where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.PullRequest (PullRequestWebhook)

mkEnum "PullRequestAction"
  [ "ASSIGNED"
  , "UNASSIGNED"
  , "REVIEW_REQUESTED"
  , "REVIEW_REQUEST_REMOVED"
  , "LABELED"
  , "UNLABELED"
  , "OPENED"
  , "EDITED"
  , "CLOSED"
  , "REOPENED"
  , "SYNCHRONIZE"
  ]

type PullRequestEvent = [schema|
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
