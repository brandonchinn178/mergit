{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{- |
Module      :  MergeBot.Core.Text
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines labels and messages used in the MergeBot.
-}
module MergeBot.Core.Text where

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GitHub.REST (KeyValue (..))
import Text.Read (readMaybe)

import MergeBot.Core.Actions (MergeBotAction (..), renderAction)

default (Text)

{- Check runs -}

-- | The label for the check run for trying PRs.
checkRunTry :: Text
checkRunTry = "Bot Try"

-- | The one-line label to display when the try check run is initially created.
tryJobLabelInit :: Text
tryJobLabelInit = "Try run not started"

-- | The summary text to display when the try check run is initially created.
tryJobSummaryInit :: Text
tryJobSummaryInit = "Click \"Run Try\" above to begin your try run."

-- | The one-line label to display when the try check run is running.
tryJobLabelRunning :: Text
tryJobLabelRunning = "Try run in progress"

-- | The one-line label to display when the try check run is completed.
tryJobLabelDone :: Text
tryJobLabelDone = "Try run finished"

-- | The summary text to display when the try check run failed.
tryJobSummaryFailed :: Text
tryJobSummaryFailed =
  Text.unwords
    [ "Try run failed. If the failures are unrelated to your changes, navigate to the Circle CI"
    , "workflow page and click \"Rerun\" > \"Rerun Workflow from Failed\". Otherwise, make the"
    , "fixes, push up a new commit, and run try on that commit."
    ]

-- | The summary text to display when the try check run was successful.
tryJobSummarySuccess :: Text
tryJobSummarySuccess =
  "To re-run try job, click the \"Run Try\" button again, **NOT** any of the \"Re-run\" links."

tryJobInitData :: UTCTime -> [KeyValue]
tryJobInitData now =
  [ "status" := "completed"
  , "conclusion" := "neutral"
  , "completed_at" := now
  , "output" := output tryJobLabelInit tryJobSummaryInit
  , "actions" := [renderAction BotTry]
  ]

-- | The label for the check run for merging PRs.
checkRunMerge :: Text
checkRunMerge = "Bot Merge"

-- | The one-line label to display when the merge check run is initially created.
mergeJobLabelInit :: Text
mergeJobLabelInit = "Not Queued"

-- | The summary text to display when the merge check run is initially created.
mergeJobSummaryInit :: Text
mergeJobSummaryInit = "Click \"Queue\" above to queue this PR for the next merge run."

-- | The one-line label to display when the merge check run is queued.
mergeJobLabelQueued :: Text
mergeJobLabelQueued = "Queued for next merge run"

-- | The summary text to display when the merge check run is queued.
mergeJobSummaryQueued :: Text
mergeJobSummaryQueued = "Click \"Dequeue\" above to remove this PR from the queue."

-- | The one-line label to display when the merge check run is running.
mergeJobLabelRunning :: Text
mergeJobLabelRunning = "Merge run in progress"

-- | The one-line label to display when the merge check run is completed.
mergeJobLabelDone :: Text
mergeJobLabelDone = "Merge run finished"

-- | The summary text to display when the merge check run failed.
mergeJobSummaryFailed :: Text
mergeJobSummaryFailed =
  Text.unwords
    [ "Merge run failed. Fix the failures, or click \"Queue\" above to re-queue the PR if the"
    , "failures are unrelated to your PR. (Do **NOT** click any of the \"Re-run\" links)"
    ]

-- | The summary text to display when the merge check run is successful.
mergeJobSummarySuccess :: Text
mergeJobSummarySuccess =
  Text.unwords
    [ "Merge run successful. If something went wrong and you need to reset this check run, click"
    , "\"Reset\" above."
    ]

mergeJobInitData :: UTCTime -> [KeyValue]
mergeJobInitData now =
  [ "status" := "completed"
  , "conclusion" := "action_required"
  , "completed_at" := now
  , "output" := output mergeJobLabelInit mergeJobSummaryInit
  , "actions" := [renderAction BotQueue]
  ]

-- | The output object for check runs.
output :: Text -> Text -> [KeyValue]
output title summary = ["title" := title, "summary" := summary]

{- CI branches -}

-- | Display the pull request number.
showId :: Int -> Text
showId = Text.pack . ('#' :) . show

-- | Parse a pull request number.
parseId :: Text -> Maybe Int
parseId s = case Text.unpack s of
  '#' : num -> readMaybe num
  _ -> Nothing

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
toTryMessage prNum = Text.unwords ["Try", showId prNum]

-- | Get the name of the staging branch for the given base branch.
toStagingBranch :: Text -> Text
toStagingBranch = ("staging-" <>)

-- | Return True if the given branch is a staging branch.
isStagingBranch :: Text -> Bool
isStagingBranch = isJust . fromStagingBranch

-- | Get the base branch for the given staging branch.
fromStagingBranch :: Text -> Maybe Text
fromStagingBranch = Text.stripPrefix "staging-"

-- | Get the commit message for the merge run for the given pull requests.
toStagingMessage :: Text -> [Int] -> Text
toStagingMessage base prs = Text.unwords $ ["Merge"] ++ map showId prs ++ ["into", base]

-- | Get the pull requests from the given staging branch message.
fromStagingMessage :: Text -> Maybe (Text, [Int])
fromStagingMessage = traverse (traverse parseId) <=< getPRsFromMessage
  where
    getPRsFromMessage :: Text -> Maybe (Text, [Text])
    getPRsFromMessage msg =
      -- Parse out ["Merge", pr1, pr2, ..., "into", "base_branch"]
      case Text.words msg of
        "Merge" : rest
          | base : "into" : prsRev <- reverse rest ->
            Just (base, reverse prsRev)
        _ -> Nothing
