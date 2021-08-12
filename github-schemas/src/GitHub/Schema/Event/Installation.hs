{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Schema.Event.Installation
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for InstallationEvent.
-}
module GitHub.Schema.Event.Installation where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Installation (Installation)
import GitHub.Schema.Repository (RepositoryShort)

mkEnum
  "InstallationAction"
  [ "CREATED"
  , "DELETED"
  , "NEW_PERMISSIONS_ACCEPTED"
  ]

type InstallationEvent =
  [schema|
  {
    action: InstallationAction,
    installation: #Installation,
    repositories: List #RepositoryShort,
    #BaseEvent,
  }
|]
