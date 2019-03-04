{-|
Module      :  Servant.GitHub.Event.Follow
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for FollowEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Follow where

import Data.Aeson.Schema (schema)

import Servant.GitHub.Event.Common

type FollowSchema = [schema|
  {
    "target": #User,
    #BaseEvent,
  }
|]
