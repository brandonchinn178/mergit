{-|
Module      :  MergeBot.Core.CheckRun
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for manipulating GitHub check runs.
-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core.CheckRun
  ( createTryCheckRun
  , createMergeCheckRun
  , CheckRunStatus(..)
  , CheckRunUpdates(..)
  , updateCheckRuns
  , updateCheckRun
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST (KeyValue(..))

import MergeBot.Core.Actions (MergeBotAction(..), renderAction)
import MergeBot.Core.GitHub (CheckRunId, createCheckRun, updateCheckRun')
import MergeBot.Core.Monad (MonadMergeBot)
import MergeBot.Core.Text

default (Text)

-- | Create the check run for trying PRs.
createTryCheckRun :: MonadMergeBot m => GitObjectID -> [KeyValue] -> m ()
createTryCheckRun sha checkRunData =
  createCheckRun $
    [ "name"         := checkRunTry
    , "head_sha"     := sha
    ] ++ checkRunData

-- | Create the check run for queuing/merging PRs.
createMergeCheckRun :: MonadMergeBot m => GitObjectID -> [KeyValue] -> m ()
createMergeCheckRun sha checkRunData =
  createCheckRun $
    [ "name"         := checkRunMerge
    , "head_sha"     := sha
    ] ++ checkRunData

data CheckRunStatus
  = CheckRunQueued
  | CheckRunInProgress
  | CheckRunComplete Bool -- ^ is successful?
  deriving (Show)

data CheckRunUpdates = CheckRunUpdates
  { isStart        :: Bool
  , isTry          :: Bool
  , checkRunStatus :: CheckRunStatus
  , checkRunBody   :: [Text] -- ^ Lines for the check run body, as markdown
  } deriving (Show)

-- | Update the given check runs with the parameters in CheckRunUpdates
updateCheckRuns :: MonadMergeBot m => [(GitObjectID, CheckRunId)] -> CheckRunUpdates -> m ()
updateCheckRuns checkRuns CheckRunUpdates{..} = do
  checkRunData <- mkCheckRunData <$> liftIO getCurrentTime
  mapM_ (doUpdateCheckRun checkRunData) checkRuns
  where
    actions = case checkRunStatus of
      CheckRunComplete True -> [BotResetMerge]
      CheckRunComplete False -> if isTry then [BotTry] else [BotQueue]
      CheckRunInProgress -> []
      CheckRunQueued -> [BotDequeue]

    jobLabel = case checkRunStatus of
      CheckRunComplete _ -> if isTry then tryJobLabelDone else mergeJobLabelDone
      CheckRunInProgress -> if isTry then tryJobLabelRunning else mergeJobLabelRunning
      CheckRunQueued -> mergeJobLabelQueued

    mkCheckRunData now = concat
      [ if isStart then [ "started_at" := now ] else []
      , case checkRunStatus of
          CheckRunComplete isSuccess ->
            [ "status"       := "completed"
            , "conclusion"   := if isSuccess then "success" else "failure"
            , "completed_at" := now
            ]
          CheckRunInProgress ->
            [ "status" := "in_progress"
            ]
          CheckRunQueued ->
            [ "status" := "queued"
            ]
      , [ "actions" := map renderAction actions
        , "output" := output jobLabel (unlines2 checkRunBody)
        ]
      ]

    doUpdateCheckRun checkRunData (sha, checkRunId) =
      updateCheckRun isStart isTry checkRunId sha checkRunData

-- | CORRECTLY update a check run.
--
-- GitHub Checks API requires creating a new CheckRun when transitioning from completed
-- status to non-completed status (undocumented, email thread between GitHub Support and
-- brandon@leapyear.io)
updateCheckRun :: MonadMergeBot m
  => Bool
     -- ^ Whether this update should completely overwrite the existing check run. REQUIRED to be
     -- True if the status transitions from completed to a non-completed status (e.g. in_progress).
  -> Bool
     -- ^ Whether the check run being updated is a try check run
  -> CheckRunId
  -> GitObjectID
  -> [KeyValue]
  -> m ()
updateCheckRun shouldOverwrite isTry checkRunId sha checkRunData
  | not shouldOverwrite = updateCheckRun' checkRunId checkRunData
  | isTry = createTryCheckRun sha checkRunData
  | otherwise = createMergeCheckRun sha checkRunData

{- Helpers -}

-- | A helper for doing 'unlines' on Markdown text (which requires 2 newlines).
unlines2 :: [Text] -> Text
unlines2 = Text.concat . map (<> "\n\n")
