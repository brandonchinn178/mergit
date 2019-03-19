{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core
  ( createCheckRuns
  , startTryJob
  , handleStatusUpdate
  ) where

import Control.Exception (displayException)
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.GraphQL (get)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Data.Yaml (decodeThrow)
import GitHub.Data.GitObjectID (GitObjectID)
import qualified GitHub.Data.StatusState as StatusState
import GitHub.REST (GitHubData, KeyValue(..))

import MergeBot.Core.Config
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
  void $ extractConfig tree

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
  CICommit{..} <- getCICommit sha checkName
  config <- extractConfig commitTree
  let ciStatus = displayCIStatus config commitContexts

  -- update the "Bot Try" check run
  let checkRunId = undefined
  updateCheckRun checkRunId $ ghData ++
    [ "output" :=
      [ "title" := "Try Run"
      , "summary" := "Try run is in progress (TODO: show CI details)"
      ]
    ]

-- | Get text containing Markdown showing a list of jobs required by the merge bot and their status
-- in CI.
displayCIStatus :: BotConfig -> [CIContext] -> Text
displayCIStatus BotConfig{requiredStatuses} contexts =
  let statuses = foldl updateStatusMap (mkStatusMap requiredStatuses) contexts
  in Text.unlines $ header ++ fromStatusMap statuses
  where
    header =
      [ "Context | Status"
      , "--------|-------"
      ]
    mkStatusMap = HashMap.fromList . map (, (StatusState.EXPECTED, Nothing))
    updateStatusMap statuses context = HashMap.adjust
      (const [get| context.(state, targetUrl) |])
      [get| context.context |]
      statuses
    fromStatusMap = HashMap.foldrWithKey (\k v -> (mkLine k v :)) []
    mkLine context (state, url) =
      let emoji = case state of
            StatusState.ERROR -> "❌"
            StatusState.EXPECTED -> "💤"
            StatusState.FAILURE -> "❗"
            StatusState.PENDING -> "⏳"
            StatusState.SUCCESS -> "✅"
          link = case url of
            Nothing -> context
            Just url' -> "[" <> context <> "](" <> url' <> ")"
      in link <> " | " <> emoji

-- | Get the configuration file for the given tree.
extractConfig :: Monad m => Tree -> m BotConfig
extractConfig tree =
  case filter isConfigFile [get| tree.entries![] |] of
    [] -> fail $ "Missing '" ++  configFile ++ "' file"
    [entry] -> case decodeThrow $ Text.encodeUtf8 [get| entry.object!.text! |] of
      Left e -> fail $ "Invalid '" ++ configFile ++ "' file: " ++ displayException e
      Right c -> return c
    _ -> fail $ "Multiple '" ++ configFile ++ "' files found?"
  where
    isConfigFile = (== configFileName) . [get| .name |]
    configFile = Text.unpack configFileName
