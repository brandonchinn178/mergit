{-|
Module      :  MergeBot.Core.Text
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines labels and messages used in the MergeBot.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.Text
  ( checkRunTry
  , checkRunMerge
  , toTryBranch
  , isTryBranch
  , fromTryBranch
  , toTryMessage
  ) where

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | The label for the check run for trying PRs.
checkRunTry :: Text
checkRunTry = "Bot Try"

-- | The label for the check run for merging PRs.
checkRunMerge :: Text
checkRunMerge = "Bot Merge"

-- | Display the pull request number.
toId :: Int -> Text
toId = Text.pack . ('#':) . show

-- | Get the name of the try branch for the given pull request.
toTryBranch :: Int -> Text
toTryBranch = ("trying-" <>) . Text.pack . show

-- | Return True if the given branch is a try branch.
isTryBranch :: Text -> Bool
isTryBranch = isJust . fromTryBranch

-- | Get the pull request for the given try branch.
fromTryBranch :: Text -> Maybe Int
fromTryBranch = readMaybe . Text.unpack <=< Text.stripPrefix "trying-"

-- | Get the try commit message for the given pull request.
toTryMessage :: Int -> Text
toTryMessage prNum = Text.unwords ["Try", toId prNum]
