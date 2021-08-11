{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
Module      :  MergeBot.Core.Actions
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines check run actions that can be requested in the MergeBot.

https://developer.github.com/v3/checks/runs/#actions-object
https://developer.github.com/apps/quickstart-guides/creating-ci-tests-with-the-checks-api/#step-25-updating-the-check-run-with-ci-test-results
-}
module MergeBot.Core.Actions (
  MergeBotAction (..),
  parseAction,
  renderAction,
) where

import Data.Text (Text)
import GitHub.REST (KeyValue (..))

default (Text)

-- | An action that renders as a button in the check run and runs a given merge bot action.
data MergeBotAction
  = -- | Available in the try check run. Starts a try job.
    BotTry
  | -- | Available in the merge check run. Queues a PR.
    BotQueue
  | -- | Available in the merge check run. Dequeues a PR.
    BotDequeue
  | -- | Available in the merge check run. Resets check run to initial state.
    BotResetMerge
  deriving (Show, Eq)

parseAction :: Text -> Maybe MergeBotAction
parseAction = \case
  "lybot_run_try" -> Just BotTry
  "lybot_queue" -> Just BotQueue
  "lybot_dequeue" -> Just BotDequeue
  "lybot_reset_merge" -> Just BotResetMerge
  _ -> Nothing

renderAction :: MergeBotAction -> [KeyValue]
renderAction BotTry =
  [ "label" := "Run Try"
  , "description" := "Start a try run"
  , "identifier" := "lybot_run_try"
  ]
renderAction BotQueue =
  [ "label" := "Queue"
  , "description" := "Queue this PR"
  , "identifier" := "lybot_queue"
  ]
renderAction BotDequeue =
  [ "label" := "Dequeue"
  , "description" := "Dequeue this PR"
  , "identifier" := "lybot_dequeue"
  ]
renderAction BotResetMerge =
  [ "label" := "Reset"
  , "description" := "Reset this check run"
  , "identifier" := "lybot_reset_merge"
  ]
