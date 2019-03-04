{-|
Module      :  Servant.GitHub.Event.Delete
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeleteEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Delete where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

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
