{-|
Module      :  GitHub.Schema.Event.Label
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for LabelEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.Label where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)
import GitHub.Schema.Label (Label)

mkEnum "LabelAction"
  [ "CREATED"
  , "EDITED"
  , "DELETED"
  ]

type LabelEvent = [schema|
  {
    action: LabelAction,
    label: #Label,
    changes: Maybe {
      name: Maybe {
        from: Text,
      },
      color: Maybe {
        from: Text,
      },
    },
    #BaseEvent,
  }
|]
