{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      :  GitHub.Schema.Event.Delete
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for DeleteEvent.
-}
module GitHub.Schema.Event.Delete where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)

mkEnum
  "DeleteRefType"
  [ "BRANCH"
  , "TAG"
  ]

type DeleteEvent =
  [schema|
  {
    ref_type: DeleteRefType,
    ref: Text,
    #BaseEvent,
  }
|]
