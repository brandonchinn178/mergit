{-|
Module      :  GitHub.Schema.Event.Fork
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for ForkEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Fork where

import Data.Aeson.Schema (schema)

import GitHub.Schema.BaseEvent (BaseEvent)

type ForkSchema = [schema|
  {
    "forkee": #Repository,
    #BaseEvent,
  }
|]
