{-|
Module      :  GitHub.Schema.Event.Issues
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for IssuesEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Issues where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data IssuesAction
  = IssuesOpened
  | IssuesEdited
  | IssuesDeleted
  | IssuesTransferred
  | IssuesPinned
  | IssuesUnpinned
  | IssuesClosed
  | IssuesReopened
  | IssuesAssigned
  | IssuesUnassigned
  | IssuesLabeled
  | IssuesUnlabeled
  | IssuesMilestoned
  | IssuesDemilestoned
  deriving (Show)

instance FromJSON IssuesAction where
  parseJSON = withText "IssuesAction" $ \case
    "opened" -> pure IssuesOpened
    "edited" -> pure IssuesEdited
    "deleted" -> pure IssuesDeleted
    "transferred" -> pure IssuesTransferred
    "pinned" -> pure IssuesPinned
    "unpinned" -> pure IssuesUnpinned
    "closed" -> pure IssuesClosed
    "reopened" -> pure IssuesReopened
    "assigned" -> pure IssuesAssigned
    "unassigned" -> pure IssuesUnassigned
    "labeled" -> pure IssuesLabeled
    "unlabeled" -> pure IssuesUnlabeled
    "milestoned" -> pure IssuesMilestoned
    "demilestoned" -> pure IssuesDemilestoned
    t -> fail $ "Bad IssuesAction: " ++ Text.unpack t

type IssuesSchema = [schema|
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
    "assignee": Maybe #User,
    "label": Maybe #Label,
    #BaseEvent,
  }
|]
