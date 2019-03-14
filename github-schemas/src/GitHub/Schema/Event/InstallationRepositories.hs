{-|
Module      :  GitHub.Schema.Event.InstallationRepositories
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for InstallationRepositoriesEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.InstallationRepositories where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data InstallationRepoAction
  = InstallationRepoAdded
  | InstallationRepoRemoved
  deriving (Show)

instance FromJSON InstallationRepoAction where
  parseJSON = withText "InstallationRepoAction" $ \case
    "added" -> pure InstallationRepoAdded
    "removed" -> pure InstallationRepoRemoved
    t -> fail $ "Bad InstallationRepoAction: " ++ Text.unpack t

data InstallationRepoSelection
  = InstallationRepoSelected
  | InstallationRepoSelectAll
  deriving (Show)

instance FromJSON InstallationRepoSelection where
  parseJSON = withText "InstallationRepoSelection" $ \case
    "selected" -> pure InstallationRepoSelected
    "all" -> pure InstallationRepoSelectAll
    t -> fail $ "Bad InstallationRepoSelection: " ++ Text.unpack t

type InstallationRepositoriesSchema = [schema|
  {
    "action": InstallationRepoAction,
    "installation": #Installation,
    "repository_selection": InstallationRepoSelection,
    "repositories_added": List #RepositoryShort,
    "repositories_removed": List #RepositoryShort,
    #BaseEvent,
  }
|]
