{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
Module      :  Mergit.Core.CheckRun
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub check runs.
-}
module Mergit.Core.CheckRun (
  createTryCheckRun,
  createMergeCheckRun,
  CheckRunStatus (..),
  CheckRunUpdates (..),
  updateCheckRuns,
  updateCheckRun,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST (KeyValue (..))

import Mergit.Core.Actions (MergitAction (..), renderAction)
import Mergit.Core.GitHub (CheckRunInfo (..), createCheckRun, updateCheckRun')
import Mergit.Core.GraphQL.Enums.CheckStatusState qualified as CheckStatusState
import Mergit.Core.Monad (MonadMergit)
import Mergit.Core.Text

default (Text)

-- | Create the check run for trying PRs.
createTryCheckRun :: MonadMergit m => GitObjectID -> [KeyValue] -> m ()
createTryCheckRun sha checkRunData =
  createCheckRun $
    [ "name" := checkRunTry
    , "head_sha" := sha
    ]
      ++ checkRunData

-- | Create the check run for queuing/merging PRs.
createMergeCheckRun :: MonadMergit m => GitObjectID -> [KeyValue] -> m ()
createMergeCheckRun sha checkRunData =
  createCheckRun $
    [ "name" := checkRunMerge
    , "head_sha" := sha
    ]
      ++ checkRunData

data CheckRunStatus
  = CheckRunQueued
  | CheckRunInProgress
  | CheckRunComplete Bool -- is successful
  deriving (Show)

data CheckRunUpdates = CheckRunUpdates
  { isStart :: Bool
  , isTry :: Bool
  , checkRunStatus :: CheckRunStatus
  , checkRunBody :: [Text]
  -- ^ Lines for the check run body, as markdown
  }
  deriving (Show)

-- | Update the given check runs with the parameters in CheckRunUpdates
updateCheckRuns :: MonadMergit m => [(GitObjectID, CheckRunInfo)] -> CheckRunUpdates -> m ()
updateCheckRuns checkRuns CheckRunUpdates{..} = do
  checkRunData <- mkCheckRunData <$> liftIO getCurrentTime
  mapM_ (doUpdateCheckRun checkRunData) checkRuns
  where
    actions = case checkRunStatus of
      CheckRunComplete isSuccess
        | isTry -> [BotTry]
        | isSuccess -> [BotResetMerge]
        | otherwise -> [BotQueue]
      CheckRunInProgress -> []
      CheckRunQueued -> [BotDequeue]

    jobLabel = case checkRunStatus of
      CheckRunComplete _ -> if isTry then tryJobLabelDone else mergeJobLabelDone
      CheckRunInProgress -> if isTry then tryJobLabelRunning else mergeJobLabelRunning
      CheckRunQueued -> mergeJobLabelQueued

    mkCheckRunData now =
      concat
        [ if isStart then ["started_at" := now] else []
        , case checkRunStatus of
            CheckRunComplete isSuccess ->
              [ "status" := "completed"
              , "conclusion" := if isSuccess then "success" else "failure"
              , "completed_at" := now
              ]
            CheckRunInProgress ->
              [ "status" := "in_progress"
              ]
            CheckRunQueued ->
              [ "status" := "queued"
              ]
        ,
          [ "actions" := map renderAction actions
          , "output" := output jobLabel (unlines2 checkRunBody)
          ]
        ]

    doUpdateCheckRun checkRunData (sha, checkRun) =
      updateCheckRun isTry checkRun checkRunStatus sha checkRunData

{- |
CORRECTLY update a check run.

GitHub Checks API requires creating a new CheckRun when transitioning from completed
status to non-completed status (undocumented, email thread between GitHub Support and
brandon@leapyear.io)
-}
updateCheckRun ::
  MonadMergit m =>
  -- | Whether the check run being updated is a try check run
  Bool ->
  -- | The CheckRun to update
  CheckRunInfo ->
  -- | The status the CheckRun is being updated to
  CheckRunStatus ->
  GitObjectID ->
  [KeyValue] ->
  m ()
updateCheckRun isTry CheckRunInfo{..} newStatus sha checkRunData
  | not shouldOverwrite = updateCheckRun' checkRunId checkRunData
  | isTry = createTryCheckRun sha checkRunData
  | otherwise = createMergeCheckRun sha checkRunData
  where
    shouldOverwrite =
      case checkRunState of
        CheckStatusState.COMPLETED ->
          case newStatus of
            CheckRunComplete{} -> False
            _ -> True
        _ -> False

{- Helpers -}

-- | A helper for doing 'unlines' on Markdown text (which requires 2 newlines).
unlines2 :: [Text] -> Text
unlines2 = Text.concat . map (<> "\n\n")
