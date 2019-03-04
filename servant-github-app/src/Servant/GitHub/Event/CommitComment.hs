{-|
Module      :  Servant.GitHub.Event.CommitComment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CommitCommentEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.CommitComment where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text
import Data.Time (UTCTime)

import Servant.GitHub.Event.Common

data CommitCommentAction
  = CommitCommentCreated
  deriving (Show)

instance FromJSON CommitCommentAction where
  parseJSON = withText "CommitCommentAction" $ \case
    "created" -> pure CommitCommentCreated
    t -> fail $ "Bad CommitCommentAction: " ++ Text.unpack t

type CommitCommentSchema = [schema|
  {
    "action": CommitCommentAction,
    "comment": {
      "url": Text,
      "html_url": Text,
      "id": Int,
      "node_id": Text,
      "user": #User,
      "position": Maybe Int,
      "line": Maybe Int,
      "path": Maybe Text,
      "commit_id": Text,
      "created_at": UTCTime,
      "updated_at": UTCTime,
      "author_association": Text,
      "body": Text,
    },
    #BaseEvent,
  }
|]
