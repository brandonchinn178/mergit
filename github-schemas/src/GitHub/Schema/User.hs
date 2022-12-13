{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      :  GitHub.Schema.User
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to users.
-}
module GitHub.Schema.User where

import Data.Aeson.Schema (schema)

import GitHub.Data.URL (URL)

-- | A user returned by normal endpoints, e.g. /user
type User =
  [schema|
  {
    login: Text,
    id: Int,
    node_id: Text,
    avatar_url: Text,
    gravatar_id: Text,
    url: Text,
    html_url: Text,
    followers_url: Text,
    following_url: Text,
    gists_url: Text,
    starred_url: Text,
    subscriptions_url: Text,
    organizations_url: Text,
    repos_url: Text,
    events_url: Text,
    received_events_url: Text,
    type: Text,
    site_admin: Bool,
    name: Maybe Text,
    company: Maybe Text,
    blog: Maybe Text,
    location: Maybe Text,
    email: Maybe Text,
    hireable: Maybe Bool,
    bio: Maybe Text,
    public_repos: Int,
    public_gists: Int,
    followers: Int,
    following: Int,
    created_at: Text,
    updated_at: Text,
  }
|]

-- | An abbreviated user, good for webhooks, and endpoints like `/repos/:owner/:repo`.
type UserShort =
  [schema|
  {
    login: Text,
    id: Int,
    node_id: Text,
    avatar_url: URL,
    gravatar_id: Text,
    url: Text,
    html_url: URL,
    followers_url: URL,
    following_url: URL,
    gists_url: URL,
    starred_url: URL,
    subscriptions_url: URL,
    organizations_url: URL,
    repos_url: URL,
    events_url: URL,
    received_events_url: URL,
    type: Text,
    site_admin: Bool,
  }
|]
