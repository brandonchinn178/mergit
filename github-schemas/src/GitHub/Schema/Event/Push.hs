{-|
Module      :  GitHub.Schema.Event.Push
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PushEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Push where

import Data.Aeson.Schema (schema)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)
import GitHub.Schema.BaseEvent (BaseEvent)

type PushEvent = [schema|
  {
    "ref": Text,
    "head": Text,
    "before": Text,
    "size": Int,
    "distinct_size": Int,
    "commits": List {
      "sha": GitObjectID,
      "message": Text,
      "author": {
        "name": Text,
        "email": Text,
      },
      "url": URL,
      "distinct": Bool,
    },
    #BaseEvent,
  }
|]
