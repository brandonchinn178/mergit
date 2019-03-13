{-|
Module      :  GitHub.Schema.Comment
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to comments.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Comment where

import Data.Aeson.Schema (schema)
import Data.Time (UTCTime)

import GitHub.Data.URL (URL)
import GitHub.Schema.User (UserWebhook)

-- | https://developer.github.com/v3/issues/comments/#get-a-single-comment
type Comment = [schema|
  {
    "id": Int,
    "node_id": Text,
    "url": Text,
    "html_url": URL,
    "body": Text,
    "user": #UserWebhook,
    "created_at": UTCTime,
    "updated_at": UTCTime,
  }
|]
