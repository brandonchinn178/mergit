{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core
  ( createCheckRun
  , startTryJob
  ) where

import Data.Text (Text)

import MergeBot.Core.GitHub
import MergeBot.Core.Monad (MonadMergeBot)
import MergeBot.Core.Text (toTryBranch, toTryMessage)

startTryJob :: MonadMergeBot m => Int -> Text -> m ()
startTryJob prNum base = createCIBranch base [prNum] tempBranchName tryBranch tryMessage
  where
    tryBranch = toTryBranch prNum
    tempBranchName = "temp-" <> tryBranch
    tryMessage = toTryMessage prNum

createCIBranch :: MonadMergeBot m => Text -> [Int] -> Text -> Text -> Text -> m ()
createCIBranch = undefined
