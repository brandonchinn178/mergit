{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core
  ( startTryJob
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import GitHub.REST (GitHubT)

import MergeBot.Core.Text (toTryBranch, toTryMessage)

startTryJob :: MonadIO m => Int -> Text -> GitHubT m ()
startTryJob prNum base = createCIBranch base [prNum] tempBranchName tryBranch tryMessage
  where
    tryBranch = toTryBranch prNum
    tempBranchName = "temp-" <> tryBranch
    tryMessage = toTryMessage prNum

createCIBranch :: Text -> [Int] -> Text -> Text -> Text -> GitHubT m ()
createCIBranch = undefined
