{-|
Module      :  GitHub.Schema.Event.PullRequest
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PullRequestEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.PullRequest where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data PullRequestAction
  = PullRequestAssigned
  | PullRequestUnassigned
  | PullRequestReviewRequested
  | PullRequestReviewRequestRemoved
  | PullRequestLabeled
  | PullRequestUnlabeled
  | PullRequestOpened
  | PullRequestEdited
  | PullRequestClosed
  | PullRequestReopened
  deriving (Show)

instance FromJSON PullRequestAction where
  parseJSON = withText "PullRequestAction" $ \case
    "assigned" -> pure PullRequestAssigned
    "unassigned" -> pure PullRequestUnassigned
    "review_requested" -> pure PullRequestReviewRequested
    "review_request_removed" -> pure PullRequestReviewRequestRemoved
    "labeled" -> pure PullRequestLabeled
    "unlabeled" -> pure PullRequestUnlabeled
    "opened" -> pure PullRequestOpened
    "edited" -> pure PullRequestEdited
    "closed" -> pure PullRequestClosed
    "reopened" -> pure PullRequestReopened
    t -> fail $ "Bad PullRequestAction: " ++ Text.unpack t

type PullRequestSchema = [schema|
  {
    "action": PullRequestAction,
    "number": Int,
    "changes": Maybe {
      "title": Maybe {
        "from": Text,
      },
      "body": Maybe {
        "from": Text,
      },
    },
    "pull_request": #PullRequest,
    #BaseEvent,
  }
|]
