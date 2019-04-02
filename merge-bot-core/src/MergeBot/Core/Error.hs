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
  , throwM
  ) where

import Control.Exception (Exception, SomeException, displayException)
import Control.Monad.Catch (throwM)
import Data.Aeson.Schema (Object)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID, unOID')
import GitHub.Schema.Event.CheckRun (CheckRunEvent)
import GitHub.Schema.Event.Push (PushEvent)

import MergeBot.Core.Config (configFileName)

type PullRequestId = Int

data BotError
  = CIBranchPushed (Object PushEvent)
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
  | NotFastForward [PullRequestId] Text
  | NotOnePRInCheckRun (Object CheckRunEvent)
  | UnapprovedPR PullRequestId

instance Exception BotError

instance Show BotError where
  show = \case
    CIBranchPushed o -> "User tried to manually create CI branch: " <> show o
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
    NotFastForward prs base -> "Could not fast forward `" <> Text.unpack base <> "` when merging PRs: " <> fromPRs prs
    NotOnePRInCheckRun o -> "Check run did not have exactly one PR: " <> show o
    UnapprovedPR prNum -> "PR #" <> show prNum <> " is not approved"
    where
      fromPRs = unwords . map (('#':) . show)

-- | Get the PRs relevant to the given BotError.
getRelevantPRs :: BotError -> [PullRequestId]
getRelevantPRs = \case
  CIBranchPushed _ -> []
  CommitForManyPRs _ prs -> prs
  CommitLacksPR _ -> []
  CommitNotPRHead pr _ -> [pr]
  ConfigFileMissing prs -> prs
  ConfigFileInvalid prs _ -> prs
  InvalidStaging prs _ -> prs
  MergeConflict prs -> prs
  MissingBaseBranch prs _ -> prs
  MissingCheckRun _ _ -> []
  MissingCheckRunPR pr _ -> [pr]
  NotFastForward prs _ -> prs
  NotOnePRInCheckRun _ -> []
  UnapprovedPR pr -> [pr]
