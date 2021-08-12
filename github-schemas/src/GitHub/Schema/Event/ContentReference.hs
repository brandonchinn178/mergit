{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  GitHub.Schema.Event.ContentReference
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for ContentReferenceEvent.
-}
module GitHub.Schema.Event.ContentReference where

import Data.Aeson.Schema (schema)
import Data.Aeson.Schema.TH (mkEnum)

import GitHub.Schema.BaseEvent (BaseEvent)

mkEnum
  "ContentReferenceAction"
  [ "CREATED"
  ]

type ContentReferenceEvent =
  [schema|
  {
    action: ContentReferenceAction,
    content_reference: {
      id: Int,
      node_id: Text,
      reference: Text,
    },
    #BaseEvent,
  }
|]
