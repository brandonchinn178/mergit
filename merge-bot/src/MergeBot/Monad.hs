{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for running GitHubT actions.
-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Monad
  ( runGitHub
  ) where

import Data.Text (Text)
import GitHub.REST
import Servant (Handler)

import MergeBot.Core.Monad (BotAppT, BotSettings(..), parseRepo, runBotAppT)

type BotApp = BotAppT Handler

-- | 'runBotAppT' with the arguments in a different order to take in 'Token' last.
runGitHub :: Text -> BotApp a -> Token -> Handler a
runGitHub repo action token = runBotAppT BotSettings{..} action
  where
    (repoOwner, repoName) = parseRepo repo
