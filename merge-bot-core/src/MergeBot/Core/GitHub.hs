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
  ( createBranch
  , createCheckRun
  , createCommit
  ) where

import Control.Monad (void)
import Data.Text (Text)
import GitHub.REST
    (GHEndpoint(..), GitHubData, KeyValue(..), StdMethod(..), (.:))

import MergeBot.Core.Monad (MonadMergeBot, queryGitHub')

-- | Create a branch.
--
-- https://developer.github.com/v3/git/refs/#create-a-reference
createBranch :: MonadMergeBot m => Text -> Text -> m ()
createBranch name sha = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/git/refs"
  , endpointVals = []
  , ghData =
    [ "ref" := "refs/heads/" <> name
    , "sha" := sha
    ]
  }

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

-- | Create a commit.
--
-- https://developer.github.com/v3/git/commits/#create-a-commit
createCommit :: MonadMergeBot m => Text -> Text -> [Text] -> m Text
createCommit message tree parents = (.: "sha") <$> queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/git/commits"
  , endpointVals = []
  , ghData =
    [ "message" := message
    , "tree"    := tree
    , "parents" := parents
    ]
  }
