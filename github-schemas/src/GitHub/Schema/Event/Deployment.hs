{-|
Module      :  GitHub.Schema.Event.Deployment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeploymentEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Deployment where

import Data.Aeson.Schema (schema)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Deployment (Deployment)
import GitHub.Schema.Repository (RepoWebhook)

type DeploymentEvent = [schema|
  {
    deployment: #Deployment,
    repository: #RepoWebhook,
    #BaseEvent,
  }
|]
