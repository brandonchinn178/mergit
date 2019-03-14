{-|
Module      :  GitHub.Schema.Event.DeploymentStatus
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeploymentStatusEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.DeploymentStatus where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Data.URL (URL)
import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Deployment (Deployment)
import GitHub.Schema.Repository (RepoWebhook)

mkEnum "DeploymentState"
  [ "PENDING"
  , "SUCCESS"
  , "FAILURE"
  , "ERROR"
  ]

type DeploymentStatusEvent = [schema|
  {
    "deployment_status": {
      "state": DeploymentState,
      "target_url": Maybe URL,
      "description": Maybe Text,
    },
    "deployment": #Deployment,
    "repository": #RepoWebhook,
    #BaseEvent,
  }
|]
