{-|
Module      :  GitHub.Schema.Event.Ping
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for PingEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Ping where

import Data.Aeson.Schema (schema)

type PingEvent = [schema|
  {
    zen: Text,
    hook: {
      app_id: Int,
    },
  }
|]
