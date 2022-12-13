{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      :  GitHub.Schema.Label
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines schemas related to labels.
-}
module GitHub.Schema.Label where

import Data.Aeson.Schema (schema)

import GitHub.Data.URL (URL)

-- | https://developer.github.com/v3/issues/labels/#get-a-single-label
type Label =
  [schema|
  {
    id: Int,
    node_id: Text,
    url: URL,
    name: Text,
    description: Maybe Text,
    color: Text,
    default: Bool,
  }
|]
