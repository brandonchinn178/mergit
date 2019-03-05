{-|
Module      :  Servant.GitHub.Event.DeploymentStatus
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeploymentStatusEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.DeploymentStatus where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

data DeploymentState
  = DeploymentPending
  | DeploymentSuccess
  | DeploymentFailure
  | DeploymentError
  deriving (Show)

instance FromJSON DeploymentState where
  parseJSON = withText "DeploymentState" $ \case
    "pending" -> pure DeploymentPending
    "success" -> pure DeploymentSuccess
    "failure" -> pure DeploymentFailure
    "error" -> pure DeploymentError
    t -> fail $ "Bad DeploymentState: " ++ Text.unpack t

type DeploymentStatusSchema = [schema|
  {
    "deployment_status": {
      "state": DeploymentState,
      "target_url": Maybe Text,
      "description": Maybe Text,
    },
    "deployment": #Deployment,
    "repository": #Repository,
    #BaseEvent,
  }
|]
