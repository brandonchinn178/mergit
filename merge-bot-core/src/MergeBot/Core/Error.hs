{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  MergeBot.Core.Error
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the errors thrown by the MergeBot.
-}
module MergeBot.Core.Error (
  BotError (..),
  getBotError,
  getRelevantPRs,
) where

import Control.Exception (Exception)
import Data.Aeson.Schema (Object)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID (..))
import GitHub.Schema.Event.CheckRun (CheckRunEvent)
import GitHub.Schema.Event.Push (PushEvent)

import MergeBot.Core.Config (configFileName)

type PullRequestId = Int

data BotError
  = AmbiguousPRForCommit GitObjectID
  | BadUpdate GitObjectID [PullRequestId] Text Text
  | CannotDetermineCheckRunPR (Object CheckRunEvent)
  | CIBranchPushed (Object PushEvent)
  | CICommitMissingParents Bool Text GitObjectID
  | CommitLacksPR GitObjectID
  | ConfigFileInvalid [PullRequestId] Text
  | ConfigFileMissing [PullRequestId]
  | InvalidStaging [PullRequestId] Text
  | MergeConflict [PullRequestId]
  | MissingBaseBranch [PullRequestId] Text
  | MissingCheckRun GitObjectID Text
  | MissingCheckRunPR PullRequestId Text
  | PRWasUpdatedDuringMergeRun [PullRequestId] [PullRequestId] [GitObjectID]
  | SomePRsMerged [PullRequestId] [PullRequestId]
  | TreeNotUpdated [PullRequestId] PullRequestId
  | UnapprovedPR PullRequestId
  deriving (Show, Eq)

instance Exception BotError

getBotError :: BotError -> Text
getBotError = \case
  AmbiguousPRForCommit sha -> "Could not determine PR for commit: `" <> unOID sha <> "`"
  BadUpdate sha prs base message ->
    Text.concat
      [ "Could not merge PRs "
      , fromPRs prs
      , " into `"
      , base
      , "` (" <> unOID sha <> "): "
      , message
      ]
  CannotDetermineCheckRunPR o -> "Cannot determine PR for check run: " <> showT o
  CIBranchPushed o -> "User tried to manually create CI branch: " <> showT o
  CICommitMissingParents isStart branch sha ->
    Text.concat
      [ "Commit `"
      , unOID sha
      , "` has no parents (on branch `"
      , branch
      , "`) when "
      , if isStart then "starting check run" else "updating check run"
      ]
  CommitLacksPR sha -> "Commit `" <> unOID sha <> "` does not have an associated pull request"
  ConfigFileInvalid prs msg -> "Merging " <> fromPRs prs <> " has an invalid `" <> configFileName <> "` config file: " <> msg
  ConfigFileMissing prs -> "Merging " <> fromPRs prs <> " lacks a `" <> configFileName <> "` config file"
  InvalidStaging _ branch -> "Invalid staging branch: " <> branch
  MergeConflict prs -> "Merge conflict: " <> fromPRs prs
  MissingBaseBranch _ branch -> "Base branch does not exist: " <> branch
  MissingCheckRun sha checkName -> "Commit `" <> unOID sha <> "` missing check run named: " <> checkName
  MissingCheckRunPR pr checkName -> "PR #" <> showT pr <> " missing check run named: " <> checkName
  PRWasUpdatedDuringMergeRun _ prNums shas ->
    case (prNums, shas) of
      ([prNum], [sha]) -> "PR #" <> showT prNum <> " was updated while the merge run was running. Expected SHA: `" <> unOID sha <> "`"
      _ -> "PRs " <> fromPRs prNums <> " were updated while the merge run was running."
  SomePRsMerged mergedPRs nonMergedPRs -> "PRs " <> fromPRs nonMergedPRs <> " found not merged while PRs " <> fromPRs mergedPRs <> " are merged"
  TreeNotUpdated _ pr ->
    Text.unlines
      [ "UNEXPECTED ERROR: Tree not updated when merging PR #" <> showT pr <> "."
      , ""
      , "Either the PR did not make any changes, or something went wrong on GitHub's end. Please notify the #merge-bot Slack channel and requeue your PR."
      , ""
      , "More information: https://leapyear.atlassian.net/browse/QA-178"
      ]
  UnapprovedPR prNum -> "PR #" <> showT prNum <> " is not approved"
  where
    fromPRs = Text.unwords . map (Text.cons '#' . showT)

    showT :: Show a => a -> Text
    showT = Text.pack . show

-- | Get the PRs that should be notified when throwing the given BotError.
getRelevantPRs :: BotError -> [PullRequestId]
getRelevantPRs = \case
  AmbiguousPRForCommit{} -> []
  BadUpdate _ prs _ _ -> prs
  CannotDetermineCheckRunPR{} -> []
  CIBranchPushed{} -> []
  CICommitMissingParents{} -> []
  CommitLacksPR{} -> []
  ConfigFileInvalid prs _ -> prs
  ConfigFileMissing prs -> prs
  InvalidStaging prs _ -> prs
  MergeConflict prs -> prs
  MissingBaseBranch prs _ -> prs
  MissingCheckRun{} -> []
  MissingCheckRunPR pr _ -> [pr]
  PRWasUpdatedDuringMergeRun allPRs _ _ -> allPRs
  SomePRsMerged mergedPRs nonMergedPRs -> mergedPRs ++ nonMergedPRs
  TreeNotUpdated allPRs _ -> allPRs
  UnapprovedPR pr -> [pr]
