{-# LANGUAGE LambdaCase #-}

{- |
Module      :  Mergit.Core.Error
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the errors thrown by Mergit.
-}
module Mergit.Core.Error (
  MergitError (..),
  getMergitError,
  getRelevantPRs,
) where

import Control.Exception (Exception)
import Data.Aeson.Schema (Object)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID (..))
import GitHub.Schema.Event.CheckRun (CheckRunEvent)
import GitHub.Schema.Event.Push (PushEvent)
import Text.Printf (printf)

import Mergit.Core.Config (configFileName)

type PullRequestId = Int

data MergitError
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

instance Exception MergitError

getMergitError :: MergitError -> Text
getMergitError =
  Text.pack . \case
    AmbiguousPRForCommit sha ->
      printf "Could not determine PR for commit: `%s`" sha
    BadUpdate sha prs base message ->
      printf "Could not merge PRs %s into `%s` (%s): %s" (fromPRs prs) base sha message
    CannotDetermineCheckRunPR o ->
      printf "Cannot determine PR for check run: %s" (show o)
    CIBranchPushed o ->
      printf "User tried to manually create CI branch: %s" (show o)
    CICommitMissingParents isStart branch sha ->
      printf "Commit `%s` has no parents (on branch `%s`) when %s" sha branch $
        if isStart then "starting check run" else "updating check run"
    CommitLacksPR sha ->
      printf "Commit `%s` does not have an associated pull request" sha
    ConfigFileInvalid prs msg ->
      printf "Merging %s has an invalid `%s` config file: %s" (fromPRs prs) configFileName msg
    ConfigFileMissing prs ->
      printf "Merging %s lacks a `%s` config file" (fromPRs prs) configFileName
    InvalidStaging _ branch ->
      printf "Invalid staging branch: %s" branch
    MergeConflict prs ->
      printf "Merge conflict: %s" (fromPRs prs)
    MissingBaseBranch _ branch ->
      printf "Base branch does not exist: %s" branch
    MissingCheckRun sha checkName ->
      printf "Commit `%s` missing check run named: %s" sha checkName
    MissingCheckRunPR pr checkName ->
      printf "PR #%d missing check run named: %s" pr checkName
    PRWasUpdatedDuringMergeRun _ prNums shas ->
      case (prNums, shas) of
        ([prNum], [sha]) ->
          printf "PR #%d was updated while the merge run was running. Expected SHA: `%s`" prNum sha
        _ ->
          printf "PRs %s were updated while the merge run was running." (fromPRs prNums)
    SomePRsMerged mergedPRs nonMergedPRs ->
      printf "PRs %s found not merged while PRs %s are merged" (fromPRs nonMergedPRs) (fromPRs mergedPRs)
    TreeNotUpdated _ pr ->
      unlines
        [ printf "UNEXPECTED ERROR: Tree not updated when merging PR #%d." pr
        , ""
        , unwords
            [ "Either the PR did not make any changes, or something went wrong on GitHub's end."
            , "Please make a GitHub issue (https://github.com/LeapYear/mergit/issues) and requeue your PR."
            ]
        , ""
        , "More information: https://github.com/LeapYear/mergit/issues/180#issuecomment-1097310669"
        ]
    UnapprovedPR prNum ->
      printf "PR #%d is not approved" prNum
  where
    fromPRs = unwords . map (printf "#%d")

-- | Get the PRs that should be notified when throwing the given MergitError.
getRelevantPRs :: MergitError -> [PullRequestId]
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
