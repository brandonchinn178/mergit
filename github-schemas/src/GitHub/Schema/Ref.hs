{-|
Module      :  GitHub.Schema.Ref
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to git references.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Ref where

import Data.Aeson.Schema (schema)

import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.Data.URL (URL)

-- | A Ref as returned by `/repos/:repoOwner/:repoName/git/refs`
type Ref = [schema|
  {
    ref: Text,
    node_id: Text,
    url: URL,
    object: {
      sha: GitObjectID,
      type: Text,
      url: URL,
    },
  }
|]
