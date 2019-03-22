{-|
Module      :  GitHub.Schema.Organization
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to organizations.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Organization where

import Data.Aeson.Schema (schema)

import GitHub.Data.URL (URL)

-- | An organization as returned by GitHub events.
type OrgWebhook = [schema|
  {
    login: Text,
    id: Int,
    node_id: Text,
    url: URL,
    repos_url: URL,
    events_url: URL,
    hooks_url: URL,
    issues_url: URL,
    members_url: URL,
    public_members_url: URL,
    avatar_url: URL,
    description: Text,
  }
|]
