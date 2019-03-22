{-|
Module      :  GitHub.Schema.Event.Status
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for StatusEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.Status where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)
import GitHub.Schema.BaseEvent (BaseEvent)

mkEnum "StatusState"
  [ "PENDING"
  , "SUCCESS"
  , "FAILURE"
  , "ERROR"
  ]

type StatusEvent = [schema|
  {
    id: Int,
    sha: GitObjectID,
    context: Text,
    description: Maybe Text,
    state: StatusState,
    target_url: Maybe URL,
    branches: List {
      name: Text,
    },
    #BaseEvent,
  }
|]
