{-|
Module      :  GitHub.Schema.Event.Installation
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for InstallationEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Installation where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data InstallationAction
  = InstallationCreated
  | InstallationDeleted
  | InstallationNewPerms
  deriving (Show)

instance FromJSON InstallationAction where
  parseJSON = withText "InstallationAction" $ \case
    "created" -> pure InstallationCreated
    "deleted" -> pure InstallationDeleted
    "new_permissions_accepted" -> pure InstallationNewPerms
    t -> fail $ "Bad InstallationAction: " ++ Text.unpack t

type InstallationSchema = [schema|
  {
    "action": InstallationAction,
    "installation": #Installation,
    "repositories": List #RepositoryShort,
    #BaseEvent,
  }
|]
