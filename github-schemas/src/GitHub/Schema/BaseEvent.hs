{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      :  GitHub.Schema.BaseEvent
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the 'BaseEvent' schema.
-}
module GitHub.Schema.BaseEvent where

import Data.Aeson.Schema (schema)

import GitHub.Schema.Organization (OrgWebhook)
import GitHub.Schema.Repository (RepoWebhook)
import GitHub.Schema.User (UserShort)

-- | Fields common to every event payload.
--
--  https://developer.github.com/webhooks/#payloads
type BaseEvent =
  [schema|
  {
    sender: #UserShort,
    repository: Maybe #RepoWebhook,
    organization: Maybe #OrgWebhook,
    installation: Maybe {
      id: Int,
    },
  }
|]
