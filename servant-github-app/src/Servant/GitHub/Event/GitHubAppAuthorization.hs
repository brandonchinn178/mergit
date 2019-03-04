{-|
Module      :  Servant.GitHub.Event.GitHubAppAuthorization
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the schema for GitHubAppAuthorizationEvent.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Servant.GitHub.Event.GitHubAppAuthorization where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema (schema)
import qualified Data.Text as Text

import Servant.GitHub.Event.Common

data GitHubAppAuthorizationAction
  = GitHubAppAuthorizationRevoked
  deriving (Show)

instance FromJSON GitHubAppAuthorizationAction where
  parseJSON = withText "GitHubAppAuthorizationAction" $ \case
    "revoked" -> pure GitHubAppAuthorizationRevoked
    t -> fail $ "Bad GitHubAppAuthorizationAction: " ++ Text.unpack t

type GitHubAppAuthorizationSchema = [schema|
  {
    "action": GitHubAppAuthorizationAction,
    #BaseEvent,
  }
|]
