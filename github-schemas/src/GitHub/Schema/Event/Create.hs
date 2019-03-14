{-|
Module      :  GitHub.Schema.Event.Create
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CreateEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Create where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data CreateRefType
  = CreateRefRepo
  | CreateRefBranch
  | CreateRefTag
  deriving (Show)

instance FromJSON CreateRefType where
  parseJSON = withText "CreateRefType" $ \case
    "repository" -> pure CreateRefRepo
    "branch" -> pure CreateRefBranch
    "tag" -> pure CreateRefTag
    t -> fail $ "Bad CreateRefType: " ++ Text.unpack t

type CreateSchema = [schema|
  {
    "ref_type": CreateRefType,
    "ref": Maybe Text,
    "master_branch": Text,
    "description": Maybe Text,
    #BaseEvent,
  }
|]
