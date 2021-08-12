{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Schema.Event.CommitComment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CommitCommentEvent.
-}
module GitHub.Schema.Event.CommitComment where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Comment (Comment)

mkEnum
  "CommitCommentAction"
  [ "CREATED"
  ]

type CommitCommentEvent =
  [schema|
  {
    action: CommitCommentAction,
    comment: {
      #Comment,
      position: Maybe Int,
      line: Maybe Int,
      path: Maybe Text,
      commit_id: Text,
      author_association: Text,
    },
    #BaseEvent,
  }
|]
