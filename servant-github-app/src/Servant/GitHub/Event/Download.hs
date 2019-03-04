{-|
Module      :  Servant.GitHub.Event.Download
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DownloadEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Download where

import Data.Aeson.Schema (schema)

import Servant.GitHub.Event.Common

type DownloadSchema = [schema|
  {
    "download": {
      "url": Text,
      "html_url": Text,
      "id": Int,
      "name": Text,
      "description": Text,
      "size": Int,
      "download_count": Int,
      "content_type": Text,
    },
    #BaseEvent,
  }
|]
