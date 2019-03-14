{-|
Module      :  GitHub.Schema.Event.IssueComment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for IssueCommentEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.IssueComment where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data IssueCommentAction
  = IssueCommentCreated
  | IssueCommentEdited
  | IssueCommentDeleted
  deriving (Show)

instance FromJSON IssueCommentAction where
  parseJSON = withText "IssueCommentAction" $ \case
    "created" -> pure IssueCommentCreated
    "edited" -> pure IssueCommentEdited
    "deleted" -> pure IssueCommentDeleted
    t -> fail $ "Bad IssueCommentAction: " ++ Text.unpack t

type IssueCommentSchema = [schema|
  {
    "action": IssueCommentAction,
    "changes": Maybe {
      "body": {
        "from": Text,
      },
    },
    "issue": #Issue,
    "comment": #Comment,
    #BaseEvent,
  }
|]
