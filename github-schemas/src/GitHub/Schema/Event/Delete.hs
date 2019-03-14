{-|
Module      :  GitHub.Schema.Event.Delete
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeleteEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GitHub.Schema.Event.Delete where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import GitHub.Schema.BaseEvent (BaseEvent)

data DeleteRefType
  = DeleteRefBranch
  | DeleteRefTag
  deriving (Show)

instance FromJSON DeleteRefType where
  parseJSON = withText "DeleteRefType" $ \case
    "branch" -> pure DeleteRefBranch
    "tag" -> pure DeleteRefTag
    t -> fail $ "Bad DeleteRefType: " ++ Text.unpack t

type DeleteSchema = [schema|
  {
    "ref_type": DeleteRefType,
    "ref": Text,
    #BaseEvent,
  }
|]
