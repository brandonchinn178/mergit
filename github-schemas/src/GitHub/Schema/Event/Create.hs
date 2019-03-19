{-|
Module      :  GitHub.Schema.Event.Create
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for CreateEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module GitHub.Schema.Event.Create where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)

mkEnum "CreateRefType"
  [ "REPO"
  , "BRANCH"
  , "TAG"
  ]

type CreateEvent = [schema|
  {
    ref_type: CreateRefType,
    ref: Maybe Text,
    master_branch: Text,
    description: Maybe Text,
    #BaseEvent,
  }
|]
