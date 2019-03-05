{-|
Module      :  Servant.GitHub.Event.Deployment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeploymentEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Deployment where

import Data.Aeson.Schema (schema)

import Servant.GitHub.Event.Common

type DeploymentSchema = [schema|
  {
    "deployment": #Deployment,
    "repository": #Repository,
    #BaseEvent,
  }
|]
