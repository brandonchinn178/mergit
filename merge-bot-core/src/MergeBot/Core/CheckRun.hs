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
  , CheckRunOptions(..)
  , updateCheckRuns
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import GitHub.Data.GitObjectID (GitObjectID)
import GitHub.REST (KeyValue(..))

import MergeBot.Core.Actions (MergeBotAction(..), renderAction)
import MergeBot.Core.GitHub (CheckRunId, createCheckRun, updateCheckRun)
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

data CheckRunOptions = CheckRunOptions
  { isStart      :: Bool
  , isComplete   :: Bool
  , isSuccess    :: Bool
  , isTry        :: Bool
  , checkRunBody :: [Text] -- ^ Lines for the check run body, as markdown
  } deriving (Show)

-- | Update the given check runs with the parameters in CheckRunOptions
updateCheckRuns :: MonadMergeBot m => [(GitObjectID, CheckRunId)] -> CheckRunOptions -> m ()
updateCheckRuns checkRuns CheckRunOptions{..} = do
  checkRunData <- mkCheckRunData <$> liftIO getCurrentTime
  mapM_ (updateCheckRun' checkRunData) checkRuns
  where
    doneActions
      | isTry = [BotTry]
      | isSuccess = [BotResetMerge]
      | otherwise = [BotQueue]
    actions = if isComplete then doneActions else []

    jobLabel = case (isComplete, isTry) of
      (False, True) -> tryJobLabelRunning
      (False, False) -> mergeJobLabelRunning
      (True, True) -> tryJobLabelDone
      (True, False) -> mergeJobLabelDone

    mkCheckRunData now = concat
      [ if isStart then [ "started_at" := now ] else []
      , if isComplete
          then
            [ "status"       := "completed"
            , "conclusion"   := if isSuccess then "success" else "failure"
            , "completed_at" := now
            ]
          else
            [ "status" := "in_progress"
            ]
      , [ "actions" := map renderAction actions
        , "output" := output jobLabel (unlines2 checkRunBody)
        ]
      ]

    updateCheckRun' checkRunData (_, checkRunId) = updateCheckRun checkRunId checkRunData

{- Helpers -}

-- | A helper for doing 'unlines' on Markdown text (which requires 2 newlines).
unlines2 :: [Text] -> Text
unlines2 = Text.concat . map (<> "\n\n")
