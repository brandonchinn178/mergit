{-|
Module      :  Servant.GitHub.Event.ContentReference
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for ContentReferenceEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.ContentReference where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

data ContentReferenceAction
  = ContentReferenceCreated
  deriving (Show)

instance FromJSON ContentReferenceAction where
  parseJSON = withText "ContentReferenceAction" $ \case
    "created" -> pure ContentReferenceCreated
    t -> fail $ "Bad ContentReferenceAction: " ++ Text.unpack t

type ContentReferenceSchema = [schema|
  {
    "action": ContentReferenceAction,
    "content_reference": {
      "id": Int,
      "node_id": Text,
      "reference": Text,
    },
    #BaseEvent,
  }
|]
