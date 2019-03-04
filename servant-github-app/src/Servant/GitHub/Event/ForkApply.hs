{-|
Module      :  Servant.GitHub.Event.ForkApply
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for ForkApplyEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.ForkApply where

import Data.Aeson.Schema (schema)

import Servant.GitHub.Event.Common

type ForkApplySchema = [schema|
  {
    "head": Text,
    "before": Text,
    "after": Text,
    #BaseEvent,
  }
|]
