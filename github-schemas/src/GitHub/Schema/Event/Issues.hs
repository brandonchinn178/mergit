{-|
Module      :  GitHub.Schema.Event.Issues
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for IssuesEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.Issues where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Issue (Issue)
import GitHub.Schema.Label (Label)
import GitHub.Schema.User (UserWebhook)

mkEnum "IssuesAction"
  [ "OPENED"
  , "EDITED"
  , "DELETED"
  , "TRANSFERRED"
  , "PINNED"
  , "UNPINNED"
  , "CLOSED"
  , "REOPENED"
  , "ASSIGNED"
  , "UNASSIGNED"
  , "LABELED"
  , "UNLABELED"
  , "MILESTONED"
  , "DEMILESTONED"
  ]

type IssuesEvent = [schema|
  {
    "action": IssuesAction,
    "issue": #Issue,
    "changes": Maybe {
      "title": {
        "from": Text,
      },
      "body": {
        "from": Text,
      },
    },
    "assignee": Maybe #UserWebhook,
    "label": Maybe #Label,
    #BaseEvent,
  }
|]
