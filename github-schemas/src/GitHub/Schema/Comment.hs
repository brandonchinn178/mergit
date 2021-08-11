{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      :  GitHub.Schema.Comment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to comments.
-}
module GitHub.Schema.Comment where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.URL (URL)
import GitHub.Schema.User (UserShort)

-- | https://developer.github.com/v3/issues/comments/#get-a-single-comment
type Comment =
  [schema|
  {
    id: Int,
    node_id: Text,
    url: Text,
    html_url: URL,
    body: Text,
    user: #UserShort,
    created_at: UTCTime,
    updated_at: UTCTime,
  }
|]
