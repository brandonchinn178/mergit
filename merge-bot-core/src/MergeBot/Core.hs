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
  ( createCheckRuns
  , startTryJob
  , handleStatusUpdate
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.GraphQL (get)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Data.Yaml (decodeThrow)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST (GitHubData, KeyValue(..))

import MergeBot.Core.Config (BotConfig, configFileName)
import MergeBot.Core.GitHub
import MergeBot.Core.Monad
import MergeBot.Core.Text

default (Text)

createCheckRuns :: MonadMergeBot m => GitObjectID -> m ()
createCheckRuns sha = createTryCheckRun sha >> createMergeCheckRun sha

-- | Create the check run for trying PRs.
createTryCheckRun :: MonadMergeBot m => GitObjectID -> m ()
createTryCheckRun sha = createCheckRun
  [ "name"     := checkRunTry
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
  [ "name"     := checkRunMerge
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
startTryJob prNum prSHA baseSHA = do
  mergeSHA <- createCIBranch baseSHA [prSHA] tryBranch tryMessage

  now <- liftIO getCurrentTime
  let ghData =
        [ "started_at" := now
        , "status"     := "in_progress"
        , "actions"    := []
        ]
  refreshCheckRuns ghData mergeSHA checkRunTry
  where
    tryBranch = toTryBranch prNum
    tryMessage = toTryMessage prNum

-- | Handle a notification that the given commit's status has been updated.
handleStatusUpdate :: MonadMergeBot m => GitObjectID -> Text -> m ()
handleStatusUpdate = refreshCheckRuns []

{- Helpers -}

-- | Create a branch for a try or merge job.
--
-- * Deletes the existing try or merge branch, if one exists.
-- * Errors if merge conflict
-- * Errors if the .lymerge.yaml file is missing or invalid
createCIBranch :: MonadMergeBot m => GitObjectID -> [GitObjectID] -> Text -> Text -> m GitObjectID
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
  tree <- getBranchTree ciBranch

  -- check missing/invalid .lymerge.yaml file
  when (isNothing $ extractConfig tree) $
    fail $ "Missing or invalid " ++ Text.unpack configFileName ++ " file."

  -- create a new commit that merges all the PRs at once
  mergeSHA <- createCommit message [get| tree.oid |] (baseSHA : prSHAs)

  -- forcibly update CI branch to point to new merge commit
  success <- updateBranch True ciBranch mergeSHA
  unless success $
    fail "Force update CI branch failed"

  return mergeSHA

-- | Update the check runs for the given CI commit, including any additional data provided.
refreshCheckRuns :: MonadMergeBot m => GitHubData -> GitObjectID -> Text -> m ()
refreshCheckRuns ghData sha checkName = do
  -- TODO: get sha commit parents, 'tail' to ignore baseSHA and get prSHAs
  checkRunId <- undefined sha checkName

  -- TODO: get .lymerge.yaml config

  -- update the "Bot Try" check run
  updateCheckRun checkRunId $ ghData ++
    [ "output" :=
      [ "title" := "Try Run"
      , "summary" := "Try run is in progress (TODO: show CI details)"
      ]
    ]

-- | Get the configuration file for the given tree.
extractConfig :: Tree -> Maybe BotConfig
extractConfig tree =
  case filter isConfigFile [get| tree.entries![] |] of
    [] -> Nothing
    [entry] -> decodeThrow $ Text.encodeUtf8 [get| entry.object!.text! |]
    _ -> error $ "Multiple '" ++ Text.unpack configFileName ++ "' files found?"
  where
    isConfigFile = (== configFileName) . [get| .name |]
