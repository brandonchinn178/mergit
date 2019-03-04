{-|
Module      :  Servant.GitHub.Event.Label
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for LabelEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.Label where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

data LabelAction
  = LabelCreated
  | LabelEdited
  | LabelDeleted
  deriving (Show)

instance FromJSON LabelAction where
  parseJSON = withText "LabelAction" $ \case
    "opened" -> pure LabelCreated
    "edited" -> pure LabelEdited
    "deleted" -> pure LabelDeleted
    t -> fail $ "Bad LabelAction: " ++ Text.unpack t

type LabelSchema = [schema|
  {
    "action": LabelAction,
    "label": #Label,
    "changes": Maybe {
      "name": Maybe {
        "from": Text,
      },
      "color": Maybe {
        "from": Text,
      },
    },
    #BaseEvent,
  }
|]
