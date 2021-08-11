{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      :  GitHub.Schema.Installation
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to installations.
-}
module GitHub.Schema.Installation where

import Data.Aeson.Schema (schema)

import GitHub.Data.URL (URL)
import GitHub.Schema.User (UserShort)

type Installation =
  [schema|
  {
    id: Int,
    account: #UserShort,
    repository_selection: Text,
    access_tokens_url: URL,
    repositories_url: URL,
    html_url: URL,
    app_id: Int,
    target_id: Int,
    target_type: Text,
    permissions: {
      checks: Text,
      metadata: Text,
    },
    events: List Text,
    created_at: Int,
    updated_at: Int,
    single_file_name: Maybe Text,
  }
|]
