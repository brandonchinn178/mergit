{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      :  GitHub.Schema.Commit
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to commits.
-}
module GitHub.Schema.Commit where

import Data.Aeson.Schema (schema)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)

type CommitShort =
  [schema|
  {
    ref: Text,
    sha: GitObjectID,
    repo: {
      id: Int,
      url: URL,
      name: Text,
    },
  }
|]
