{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Schema.Event.GitHubAppAuthorization
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for GitHubAppAuthorizationEvent.
-}
module GitHub.Schema.Event.GitHubAppAuthorization where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)

mkEnum
  "GitHubAppAuthorizationAction"
  [ "REVOKED"
  ]

type GitHubAppAuthorizationEvent =
  [schema|
  {
    action: GitHubAppAuthorizationAction,
    #BaseEvent,
  }
|]
