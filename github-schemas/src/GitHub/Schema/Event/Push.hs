{-|
Module      :  GitHub.Schema.Event.Push
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PushEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Push where

import Data.Aeson.Schema (schema)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)
import GitHub.Schema.BaseEvent (BaseEvent)

type PushEvent = [schema|
  {
    ref: Text,
    base_ref: Maybe Text,
    after: GitObjectID,
    before: GitObjectID,
    created: Bool,
    deleted: Bool,
    commits: List {
      id: GitObjectID,
      message: Text,
      author: {
        name: Text,
        email: Text,
        username: Text,
      },
      url: URL,
      distinct: Bool,
    },
    #BaseEvent,
  }
|]
