{-|
Module      :  MergeBot.Core.Text
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines labels and messages used in the MergeBot.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.Text where

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST (KeyValue(..))
import Text.Read (readMaybe)

{- Check runs -}

-- | The label for the check run for trying PRs.
checkRunTry :: Text
checkRunTry = "Bot Try"

-- | The title of the try check run.
--
-- This is used for the title in the check run tab and also for the job
-- GitHub displays as "In progress".
tryJobTitle :: Text
tryJobTitle = "Try Run"

-- | The initial message when the try check run is created.
tryJobInitialMsg :: Text
tryJobInitialMsg = "No try run available. Click \"Run Try\" above to begin your try run."

-- | The output object for the try check run.
tryJobOutput :: Text -> [KeyValue]
tryJobOutput summary = [ "title" := tryJobTitle, "summary" := summary ]

-- | The label for the check run for merging PRs.
checkRunMerge :: Text
checkRunMerge = "Bot Merge"

-- | The title of the merge check run.
--
-- This is used for the title in the check run tab and also for the job
-- GitHub displays as "In progress".
mergeJobTitle :: Text
mergeJobTitle = "Merge Run"

-- | The initial message when the merge check run is created.
mergeJobInitialMsg :: Text
mergeJobInitialMsg = "Not queued. Click \"Queue\" above to queue this PR for the next merge run."

-- | The output object for the merge check run.
mergeJobOutput :: Text -> [KeyValue]
mergeJobOutput summary = [ "title" := mergeJobTitle, "summary" := summary ]

{- CI branches -}

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
