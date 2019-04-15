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
  ( CheckRunOptions(..)
  , updateCheckRuns
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import GitHub.REST (KeyValue(..))

import MergeBot.Core.Actions (MergeBotAction(..), renderAction)
import MergeBot.Core.GitHub (CheckRunId, updateCheckRun)
import MergeBot.Core.Monad (MonadMergeBot)
import MergeBot.Core.Text

default (Text)

data CheckRunOptions = CheckRunOptions
  { isStart      :: Bool
  , isComplete   :: Bool
  , isSuccess    :: Bool
  , isTry        :: Bool
  , checkRunBody :: [Text] -- ^ Lines for the check run body, as markdown
  } deriving (Show)

updateCheckRuns :: MonadMergeBot m => [CheckRunId] -> CheckRunOptions -> m ()
updateCheckRuns checkRunIds CheckRunOptions{..} = do
  now <- liftIO getCurrentTime

  let pendingActions
        | isTry = [BotCancelTry]
        | otherwise = [BotCancelMerge]
      doneActions
        | isTry = [BotTry]
        | isSuccess = [BotResetMerge]
        | otherwise = [BotQueue]
      actions = if isComplete then doneActions else pendingActions

      jobLabel = case (isComplete, isTry) of
        (False, True) -> tryJobLabelRunning
        (False, False) -> mergeJobLabelRunning
        (True, True) -> tryJobLabelDone
        (True, False) -> mergeJobLabelDone

      checkRunData = concat
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

  mapM_ (`updateCheckRun` checkRunData) checkRunIds

{- Helpers -}

-- | A helper for doing 'unlines' on Markdown text (which requires 2 newlines).
unlines2 :: [Text] -> Text
unlines2 = Text.concat . map (<> "\n\n")
