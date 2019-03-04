{-|
Module      :  Servant.GitHub.Event.Fork
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for ForkEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Fork where

import Data.Aeson.Schema (schema)

import Servant.GitHub.Event.Common

type ForkSchema = [schema|
  {
    "forkee": #Repository,
    #BaseEvent,
  }
|]
