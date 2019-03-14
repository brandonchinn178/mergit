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

type DeploymentSchema = [schema|
  {
    "deployment": #Deployment,
    "repository": #Repository,
    #BaseEvent,
  }
|]
