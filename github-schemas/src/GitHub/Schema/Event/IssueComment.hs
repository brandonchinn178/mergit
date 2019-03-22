{-|
Module      :  GitHub.Schema.Event.IssueComment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for IssueCommentEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.IssueComment where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Comment (Comment)
import GitHub.Schema.Issue (Issue)

mkEnum "IssueCommentAction"
  [ "CREATED"
  , "EDITED"
  , "DELETED"
  ]

type IssueCommentEvent = [schema|
  {
    action: IssueCommentAction,
    changes: Maybe {
      body: {
        from: Text,
      },
    },
    issue: #Issue,
    comment: #Comment,
    #BaseEvent,
  }
|]
