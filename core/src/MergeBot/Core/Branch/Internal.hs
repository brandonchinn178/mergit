{-|
Module      :  MergeBot.Core.Branch.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Internal functions for mergebot branch management.
-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.Branch.Internal where

import Control.Monad ((<=<))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

import MergeBot.Core.Data (PullRequestId)

-- | Display the pull request number.
toId :: PullRequestId -> Text
toId = Text.pack . ('#':) . show

-- | Get the name of the try branch for the given pull request.
toTryBranch :: PullRequestId -> Text
toTryBranch = ("trying-" <>) . Text.pack . show

-- | Get the pull request for the given try branch.
fromTryBranch :: Text -> Maybe PullRequestId
fromTryBranch = readMaybe . Text.unpack <=< Text.stripPrefix "trying-"

-- | Get the try commit message for the given pull request.
toTryMessage :: PullRequestId -> Text
toTryMessage prNum = Text.unwords ["Try", toId prNum]

-- | Get the name of the staging branch for the given base branch.
toStagingBranch :: Text -> Text
toStagingBranch = ("staging-" <>)

-- | Check if the given branch is a staging branch.
isStagingBranch :: Text -> Bool
isStagingBranch = ("staging-" `Text.isPrefixOf`)

-- | Get the message for the staging branch.
toStagingMessage :: [PullRequestId] -> Text
toStagingMessage = Text.unwords . ("Merge":) . map toId

-- | Get the pull requests from the given message.
fromStagingMessage :: Text -> [PullRequestId]
fromStagingMessage = map (read . tail . Text.unpack) . tail . Text.words
