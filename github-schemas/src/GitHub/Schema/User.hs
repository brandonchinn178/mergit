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

-- | A user returned by normal endpoints, e.g. /user
type User = [schema|
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
    name: Text,
    company: Maybe Text,
    blog: Text,
    location: Maybe Text,
    email: Text,
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

-- | A user as returned by GitHub events.
type UserWebhook = [schema|
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
