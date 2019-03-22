{-|
Module      :  GitHub.Schema.Team
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to teams.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Team where

import Data.Aeson.Schema (schema)

import GitHub.Data.TeamPrivacy (TeamPrivacy)
import GitHub.Data.URL (URL)

type Team = [schema|
  {
    id: Int,
    node_id: Text,
    url: URL,
    name: Text,
    slug: Text,
    description: Text,
    privacy: TeamPrivacy,
    permission: Text,
    members_url: URL,
    repositories_url: URL,
    parent: Maybe Int,
  }
|]
