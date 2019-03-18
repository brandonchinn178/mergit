{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core
  ( createTryCheckRun
  , createMergeCheckRun
  , startTryJob
  ) where

import Control.Monad (forM_, unless)
import Data.GraphQL (get)
import Data.Text (Text)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST (KeyValue(..))

import MergeBot.Core.GitHub
import MergeBot.Core.Monad (MonadMergeBot(..))
import MergeBot.Core.Text (toTryBranch, toTryMessage)

default (Text)

-- | Create the check run for trying PRs.
createTryCheckRun :: MonadMergeBot m => GitObjectID -> m ()
createTryCheckRun sha = createCheckRun
  [ "name"     := "Bot Try"
  , "head_sha" := sha
  , "output" :=
    [ "title"   := "Try Run"
    , "summary" := "No try run available. Click \"Run Try\" above to begin your try run."
    ]
  , "actions" :=
    [ [ "label"       := "Run Try"
      , "description" := "Start a try run"
      , "identifier"  := "lybot_run_try"
      ]
    ]
  ]

-- | Create the check run for queuing/merging PRs.
createMergeCheckRun :: MonadMergeBot m => GitObjectID -> m ()
createMergeCheckRun sha = createCheckRun
  [ "name"     := "Bot Merge"
  , "head_sha" := sha
  , "output" :=
    [ "title"   := "Merge Run"
    , "summary" := "Not queued. Click \"Queue\" above to queue this PR for the next merge run."
    ]
  , "actions" :=
    [ [ "label"       := "Queue"
      , "description" := "Queue this PR"
      , "identifier"  := "lybot_queue"
      ]
    ]
  ]

-- | Start a new try job.
startTryJob :: MonadMergeBot m => Int -> GitObjectID -> GitObjectID -> m ()
startTryJob prNum prSHA baseSHA = createCIBranch baseSHA [prSHA] tryBranch tryMessage
  where
    tryBranch = toTryBranch prNum
    tryMessage = toTryMessage prNum

{- Helpers -}

-- | Create a branch for a try or merge job.
--
-- * Deletes the existing try or merge branch, if one exists.
-- * Errors if merge conflict
-- * Errors if the .lymerge.yaml file is missing or invalid
createCIBranch :: MonadMergeBot m => GitObjectID -> [GitObjectID] -> Text -> Text -> m ()
createCIBranch baseSHA prSHAs ciBranch message = do
  deleteBranch ciBranch

  -- create CI branch off base
  createBranch ciBranch baseSHA

  -- merge prs into temp branch
  forM_ prSHAs $ \prSHA -> do
    success <- mergeBranches ciBranch prSHA "[ci skip] merge into temp"
    unless success $
      fail "Merge conflict" -- TODO: better error throwing

  -- get tree for temp branch
  tree <- getTree ciBranch

  -- TODO: fail if missing/invalid .lymerge.yaml

  -- create a new commit that merges all the PRs at once
  mergeSHA <- createCommit message [get| tree.oid |] (baseSHA : prSHAs)

  -- forcibly update CI branch to point to new merge commit
  success <- updateBranch True ciBranch mergeSHA
  unless success $
    fail "Force update CI branch failed"
