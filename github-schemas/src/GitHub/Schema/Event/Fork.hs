{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      :  GitHub.Schema.Event.Fork
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for ForkEvent.
-}
module GitHub.Schema.Event.Fork where

import Data.Aeson.Schema (schema)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Repository (RepoWebhook)

type ForkEvent =
  [schema|
  {
    forkee: #RepoWebhook,
    #BaseEvent,
  }
|]
