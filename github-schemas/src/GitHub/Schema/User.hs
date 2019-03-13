{-|
Module      :  GitHub.Schema.User
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to users.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.User where

import Data.Aeson.Schema (schema)

import GitHub.Data.URL (URL)

-- | A user as returned by GitHub events.
type UserWebhook = [schema|
  {
    "login": Text,
    "id": Int,
    "node_id": Text,
    "avatar_url": URL,
    "gravatar_id": Text,
    "url": Text,
    "html_url": URL,
    "followers_url": URL,
    "following_url": URL,
    "gists_url": URL,
    "starred_url": URL,
    "subscriptions_url": URL,
    "organizations_url": URL,
    "repos_url": URL,
    "events_url": URL,
    "received_events_url": URL,
    "type": Text,
    "site_admin": Bool,
  }
|]
