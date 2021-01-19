{-|
Module      :  MergeBot.Core.Error
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the errors thrown by the MergeBot.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.Error
  ( BotError(..)
  , getRelevantPRs
  ) where

import Control.Exception (Exception, SomeException, displayException)
import Data.Aeson.Schema (Object)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID, unOID')
import GitHub.Schema.Event.CheckRun (CheckRunEvent)
import GitHub.Schema.Event.CheckSuite (CheckSuiteEvent)
import GitHub.Schema.Event.Push (PushEvent)

import MergeBot.Core.Config (configFileName)

type PullRequestId = Int

data BotError
  = BadUpdate GitObjectID [PullRequestId] Text Text
  | CIBranchPushed (Object PushEvent)
  | CICommitMissingParents Bool Text GitObjectID
  | CommitForManyPRs GitObjectID [PullRequestId]
  | CommitLacksPR GitObjectID
  | CommitNotPRHead PullRequestId GitObjectID
  | ConfigFileMissing [PullRequestId]
  | ConfigFileInvalid [PullRequestId] SomeException
  | InvalidStaging [PullRequestId] Text
  | MergeConflict [PullRequestId]
  | MissingBaseBranch [PullRequestId] Text
  | MissingCheckRun GitObjectID Text
  | MissingCheckRunPR PullRequestId Text
  | NotOnePRInCheckRun (Object CheckRunEvent)
  | NotOnePRInCheckSuite (Object CheckSuiteEvent)
  | SomePRsMerged [PullRequestId] [PullRequestId]
  | UnapprovedPR PullRequestId
  | TreeNotUpdated [PullRequestId] PullRequestId
  | PRWasUpdatedDuringMergeRun [PullRequestId] PullRequestId GitObjectID

instance Exception BotError

instance Show BotError where
  show = \case
    BadUpdate sha prs base message -> concat
      [ "Could not merge PRs "
      , fromPRs prs
      , " into `"
      , Text.unpack base
      , "` (" ++ unOID' sha ++ "): "
      , Text.unpack message
      ]
    CIBranchPushed o -> "User tried to manually create CI branch: " <> show o
    CICommitMissingParents isStart branch sha -> concat
      [ "Commit `"
      , unOID' sha
      , "` has no parents (on branch `"
      , Text.unpack branch
      ,  "`) when "
      , if isStart then "starting check run" else "updating check run"
      ]
    CommitForManyPRs sha prs -> "Commit `" <> unOID' sha <> "` found as HEAD for multiple PRs: " <> fromPRs prs
    CommitLacksPR sha -> "Commit `" <> unOID' sha <> "` does not have an associated pull request"
    CommitNotPRHead pr sha -> "Commit `" <> unOID' sha <> "` is not HEAD for PR #" <> show pr
    ConfigFileMissing prs -> "Merging " <> fromPRs prs <> " lacks a `" <> Text.unpack configFileName <> "` config file"
    ConfigFileInvalid prs e -> "Merging " <> fromPRs prs <> " has an invalid `" <> Text.unpack configFileName <> "` config file: " <> displayException e
    InvalidStaging _ branch -> "Invalid staging branch: " <> Text.unpack branch
    MergeConflict prs -> "Merge conflict: " <> fromPRs prs
    MissingBaseBranch _ branch -> "Base branch does not exist: " <> Text.unpack branch
    MissingCheckRun sha checkName -> "Commit `" <> unOID' sha <> "` missing check run named: " <> Text.unpack checkName
    MissingCheckRunPR pr checkName -> "PR #" <> show pr <> " missing check run named: " <> Text.unpack checkName
    NotOnePRInCheckRun o -> "Check run did not have exactly one PR: " <> show o
    NotOnePRInCheckSuite o -> "Check suite did not have exactly one PR: " <> show o
    SomePRsMerged mergedPRs nonMergedPRs -> "PRs " <> fromPRs nonMergedPRs <> " found not merged while PRs " <> fromPRs mergedPRs <> " are merged"
    UnapprovedPR prNum -> "PR #" <> show prNum <> " is not approved"
    TreeNotUpdated _ pr -> unlines
      [ "UNEXPECTED ERROR: Tree not updated when merging PR #" <> show pr <> "."
      , ""
      , "Either the PR did not make any changes, or something went wrong on GitHub's end. Please notify the #merge-bot Slack channel and requeue your PR."
      , ""
      , "More information: https://leapyear.atlassian.net/browse/QA-178"
      ]
    PRWasUpdatedDuringMergeRun _ prNum sha -> "PR #" <> show prNum <> " was updated while the merge run was running. Expected SHA: `" <> unOID' sha <> "`"
    where
      fromPRs = unwords . map (('#':) . show)

-- | Get the PRs that should be notified when throwing the given BotError.
getRelevantPRs :: BotError -> [PullRequestId]
getRelevantPRs = \case
  BadUpdate _ prs _ _ -> prs
  CIBranchPushed{} -> []
  CICommitMissingParents{} -> []
  CommitForManyPRs _ prs -> prs
  CommitLacksPR{} -> []
  CommitNotPRHead pr _ -> [pr]
  ConfigFileMissing prs -> prs
  ConfigFileInvalid prs _ -> prs
  InvalidStaging prs _ -> prs
  MergeConflict prs -> prs
  MissingBaseBranch prs _ -> prs
  MissingCheckRun{} -> []
  MissingCheckRunPR pr _ -> [pr]
  NotOnePRInCheckRun{} -> []
  NotOnePRInCheckSuite{} -> []
  SomePRsMerged mergedPRs nonMergedPRs -> mergedPRs ++ nonMergedPRs
  UnapprovedPR pr -> [pr]
  TreeNotUpdated allPRs _ -> allPRs
  PRWasUpdatedDuringMergeRun allPRs _ _ -> allPRs
