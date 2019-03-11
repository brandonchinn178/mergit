{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for running GitHubT actions.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Monad
  ( runGitHub
  , createCheckRun
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
import Servant (Handler)

type GitHubM = GitHubT Handler

runGitHub :: GitHubM a -> Token -> Handler a
runGitHub action token = runGitHubT state action
  where
    state = GitHubState
      { token
      , userAgent = "LeapYear/merge-bot"
      , apiVersion = "antiope-preview"
      }

createCheckRun :: Text -> GitHubData -> GitHubM ()
createCheckRun repo ghData = queryGitHub_ GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/check-runs"
  , endpointVals = parseRepo repo
  , ghData
  }

{- Helpers -}

parseRepo :: Text -> [KeyValue]
parseRepo repo = case Text.splitOn "/" repo of
  [repoOwner, repoName] -> [ "owner" := repoOwner, "repo" := repoName ]
  _ -> error $ "Invalid repo: " ++ Text.unpack repo
