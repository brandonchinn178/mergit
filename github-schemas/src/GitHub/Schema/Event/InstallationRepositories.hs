{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Schema.Event.InstallationRepositories
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for InstallationRepositoriesEvent.
-}
module GitHub.Schema.Event.InstallationRepositories where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Installation (Installation)
import GitHub.Schema.Repository (RepositoryShort)

mkEnum
  "InstallationRepoAction"
  [ "ADDED"
  , "REMOVED"
  ]

mkEnum
  "InstallationRepoSelection"
  [ "SELECTED"
  , "ALL"
  ]

type InstallationRepositoriesEvent =
  [schema|
  {
    action: InstallationRepoAction,
    installation: #Installation,
    repository_selection: InstallationRepoSelection,
    repositories_added: List #RepositoryShort,
    repositories_removed: List #RepositoryShort,
    #BaseEvent,
  }
|]
