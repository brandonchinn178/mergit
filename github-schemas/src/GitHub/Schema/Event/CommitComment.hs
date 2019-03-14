{-|
Module      :  GitHub.Schema.Event.CommitComment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CommitCommentEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.CommitComment where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

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
      #Comment,
      "position": Maybe Int,
      "line": Maybe Int,
      "path": Maybe Text,
      "commit_id": Text,
      "author_association": Text,
    },
    #BaseEvent,
  }
|]
