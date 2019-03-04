{-|
Module      :  Servant.GitHub.Event.Installation
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for InstallationEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Installation where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

data InstallationAction
  = InstallationCreated
  | InstallationDeleted
  deriving (Show)

instance FromJSON InstallationAction where
  parseJSON = withText "InstallationAction" $ \case
    "created" -> pure InstallationCreated
    "deleted" -> pure InstallationDeleted
    t -> fail $ "Bad InstallationAction: " ++ Text.unpack t

type InstallationSchema = [schema|
  {
    "action": InstallationAction,
    "installation": #Installation,
    "repositories": List #RepositoryShort,
    #BaseEvent,
  }
|]
