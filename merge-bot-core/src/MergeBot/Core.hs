{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core
  ( createTryCheckRun
  , createMergeCheckRun
  , startTryJob
  ) where

import Data.Text (Text)
import GitHub.REST (KeyValue(..))

import MergeBot.Core.GitHub
import MergeBot.Core.Monad (MonadMergeBot)
import MergeBot.Core.Text (toTryBranch, toTryMessage)

default (Text)

-- | Create the check run for trying PRs.
createTryCheckRun :: MonadMergeBot m => Text -> m ()
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
createMergeCheckRun :: MonadMergeBot m => Text -> m ()
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

startTryJob :: MonadMergeBot m => Int -> Text -> m ()
startTryJob prNum base = createCIBranch base [prNum] tempBranchName tryBranch tryMessage
  where
    tryBranch = toTryBranch prNum
    tempBranchName = "temp-" <> tryBranch
    tryMessage = toTryMessage prNum

createCIBranch :: MonadMergeBot m => Text -> [Int] -> Text -> Text -> Text -> m ()
createCIBranch = undefined
