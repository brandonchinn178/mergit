{-|
Module      :  MergeBot.Core.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub state.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.GitHub
  ( createCheckRun
  ) where

import Control.Monad (void)
import GitHub.REST (GHEndpoint(..), GitHubData, StdMethod(..))

import MergeBot.Core.Monad (MonadMergeBot, queryGitHub')

-- | Create a check run.
--
-- https://developer.github.com/v3/checks/runs/#create-a-check-run
createCheckRun :: MonadMergeBot m => GitHubData -> m ()
createCheckRun ghData = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/check-runs"
  , endpointVals = []
  , ghData
  }
