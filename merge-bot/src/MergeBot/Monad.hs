{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for running GitHubT actions.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Monad
  ( runBotApp
  ) where

import Data.Aeson.Schema (Object, get)
import GitHub.REST
import GitHub.Schema.Repository (RepoWebhook)
import Servant (Handler)

import MergeBot.Core.Monad (BotAppT, BotSettings(..), parseRepo, runBotAppT)

type BotApp = BotAppT Handler

-- | A helper around 'runBotAppT' for easy use by the Servant handlers.
runBotApp :: Object RepoWebhook -> BotApp a -> Token -> Handler a
runBotApp repo action token = runBotAppT BotSettings{..} action
  where
    (repoOwner, repoName) = parseRepo [get| repo.full_name |]
