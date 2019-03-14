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

import GitHub.Schema.BaseEvent (BaseEvent)

type PushSchema = [schema|
  {
    "ref": Text,
    "head": Text,
    "before": Text,
    "size": Int,
    "distinct_size": Int,
    "commits": List {
      "sha": SHA,
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
