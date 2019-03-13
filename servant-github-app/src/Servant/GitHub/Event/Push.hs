{-|
Module      :  Servant.GitHub.Event.Push
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PushEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Push where

import Data.Aeson.Schema (schema)

import Servant.GitHub.Event.Common

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
