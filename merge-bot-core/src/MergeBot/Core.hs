{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Core
  ( createCheckRuns
  , startTryJob
  , queuePR
  , dequeuePR
  , resetMerge
  , handleStatusUpdate
  , pollQueues
  ) where

import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (whileM_)
import Data.Bifunctor (first)
import Data.GraphQL (get)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Data.Yaml (decodeThrow)
import GitHub.Data.GitObjectID (GitObjectID)
import qualified GitHub.Data.PullRequestReviewState as PullRequestReviewState
import qualified GitHub.Data.StatusState as StatusState
import UnliftIO.Exception (finally, fromEither, onException, throwIO)

import MergeBot.Core.CheckRun
import MergeBot.Core.Config
import MergeBot.Core.Error
import MergeBot.Core.GitHub
import MergeBot.Core.Monad
import MergeBot.Core.Status
import MergeBot.Core.Text

default (Text)

createCheckRuns :: MonadMergeBot m => GitObjectID -> m ()
createCheckRuns sha = do
  now <- liftIO getCurrentTime
  createTryCheckRun sha $ tryJobInitData now
  createMergeCheckRun sha $ mergeJobInitData now

-- | Start a new try job.
startTryJob :: MonadMergeBot m => Int -> GitObjectID -> Text -> CheckRunId -> m ()
startTryJob prNum prSHA base checkRunId = do
  mergeSHA <-
    createCIBranch base [prSHA] tryBranch tryMessage
      `onException` updateCheckRuns [(prSHA, checkRunId)] CheckRunUpdates
        { isStart = True
        , isTry = True
        , checkRunStatus = CheckRunComplete False
        , checkRunBody = ["Unable to start try job."]
        }

  refreshCheckRuns True tryBranch mergeSHA
  where
    tryBranch = toTryBranch prNum
    tryMessage = toTryMessage prNum

-- | Add a PR to the queue.
queuePR :: MonadMergeBot m => Int -> GitObjectID -> m ()
queuePR prNum sha = do
  -- TODO: lookup how many approvals are required
  reviews <- getPRReviews prNum

  unless (PullRequestReviewState.APPROVED `elem` reviews) $
    throwIO $ UnapprovedPR prNum

  checkRunId <- getCheckRun prNum checkRunMerge
  -- TOOD: batching info
  updateCheckRuns [(sha, checkRunId)] CheckRunUpdates
    { isStart = True -- previous status of merge check run was "Complete: Action Required"
    , isTry = False
    , checkRunStatus = CheckRunQueued
    , checkRunBody = [mergeJobSummaryQueued]
    }

-- | Remove a PR from the queue.
dequeuePR :: MonadMergeBot m => Int -> GitObjectID -> m ()
dequeuePR = resetMerge

-- | Remove a PR from the queue.
resetMerge :: MonadMergeBot m => Int -> GitObjectID -> m ()
resetMerge _ sha = do
  now <- liftIO getCurrentTime
  -- reset check run by completely re-creating it
  createMergeCheckRun sha $ mergeJobInitData now

-- | Handle a notification that the given commit's status has been updated.
handleStatusUpdate :: MonadMergeBot m => Text -> GitObjectID -> m ()
handleStatusUpdate = refreshCheckRuns False

-- | Load all queues and start a merge run if one is not already running.
pollQueues :: MonadMergeBot m => m ()
pollQueues = do
  queues <- getQueues
  void $ flip HashMap.traverseWithKey queues $ \base prs -> do
    staging <- getBranchSHA $ toStagingBranch base
    when (isNothing staging) $ startMergeJob prs base
  where
    startMergeJob prs base = do
      let (prNums, prSHAs, checkRunIds) = unzip3 prs
          stagingBranch = toStagingBranch base
          stagingMessage = toStagingMessage base prNums
      mergeSHA <- createCIBranch base prSHAs stagingBranch stagingMessage
        `onException` updateCheckRuns (zip prSHAs checkRunIds) CheckRunUpdates
          { isStart = True
          , isTry = False
          , checkRunStatus = CheckRunComplete False
          , checkRunBody = ["Unable to start merge job."]
          }

      refreshCheckRuns True stagingBranch mergeSHA

{- Helpers -}

-- | Create a branch for a try or merge job.
--
-- * Deletes the existing try or merge branch, if one exists.
-- * Errors if merge conflict
-- * Errors if the .lymerge.yaml file is missing or invalid
createCIBranch :: MonadMergeBot m => Text -> [GitObjectID] -> Text -> Text -> m GitObjectID
createCIBranch base prSHAs ciBranch message = do
  deleteBranch ciBranch
  deleteBranch tempBranch

  (`finally` deleteBranch tempBranch) $ do
    prNums <- mapM (fmap prForCommitId . getPRForCommit) prSHAs

    baseSHA <- maybe (throwIO $ MissingBaseBranch prNums base) return =<< getBranchSHA base

    -- create temp branch off base
    createBranch tempBranch baseSHA

    -- merge prs into temp branch
    forM_ prSHAs $ \prSHA -> do
      success <- mergeBranches tempBranch prSHA "[ci skip] merge into temp"
      unless success $ throwIO $ MergeConflict prNums

    -- get tree for temp branch
    tree <- getBranchTree tempBranch

    -- check missing/invalid .lymerge.yaml file
    void $ fromEither $ extractConfig prNums tree

    -- create a new commit that merges all the PRs at once
    mergeSHA <- createCommit message [get| tree.oid |] (baseSHA : prSHAs)

    -- forcibly update CI branch to point to new merge commit
    createBranch ciBranch mergeSHA

    return mergeSHA
  where
    tempBranch = "temp-" <> ciBranch

-- | Refresh the check runs for the given CI commit.
refreshCheckRuns :: MonadMergeBot m => Bool -> Text -> GitObjectID -> m ()
refreshCheckRuns isStart ciBranchName sha = do
  CICommit{..} <- getCICommit sha checkName
  when (null parents) $ throwIO $ CICommitMissingParents isStart ciBranchName sha

  -- since we check the config in 'createCIBranch', we know that 'extractConfig' here will not fail
  config <-
    either (error "extractConfig failed in refreshCheckRuns") return $
      extractConfig [] commitTree

  let ciStatus = getCIStatus config commitContexts
      checkRunState = resolveCIStatus ciStatus

      -- NB: isComplete means that the merge bot should consider a check run "done"; it does not
      -- necessarily mean that CI is done. Meaning we should not clean up yet, since more CI jobs
      -- could start and fail to checkout.
      (isComplete, isSuccess) = case checkRunState of
        StatusState.SUCCESS -> (True, True)
        StatusState.ERROR -> (True, False)
        StatusState.FAILURE -> (True, False)
        _ -> (False, False)

  ciBranchUrl <- mkCIBranchUrl
  updateCheckRuns parents CheckRunUpdates
    { isStart
    , isTry
    , checkRunStatus = if isComplete
        then CheckRunComplete isSuccess
        else CheckRunInProgress
    , checkRunBody =
        let ciInfo = "CI running in the [" <> ciBranchName <> "](" <> ciBranchUrl <> ") branch."
            message
              | not isComplete = ciInfo
              | isTry          = tryJobSummaryDone
              | not isSuccess  = mergeJobSummaryFailed
              | otherwise      = mergeJobSummarySuccess
        in [message, displayCIStatus ciStatus]
    }

  -- Post-run actions
  if
    -- If check run isn't finished yet, no post-run actions to run
    | not isComplete -> return ()
    -- If finished check run is a try, no post-run actions to run. PRs should not
    -- be merged yet, and trying branch should not be cleaned up:
    --   * If job A fails, but job B hasn't started yet (and doesn't depend on job A),
    --     we should allow job B to checkout code + run more tests
    --   * If all jobs succeed, we should allow non-blocking jobs (e.g. a deploy step)
    --     to also checkout code, so we can't delete the branch yet
    | isTry -> return ()
    -- At this point, the run is a completed merge run
    | otherwise -> do
        -- get pr information for parent commits
        let parentSHAs = map fst parents
        prs <- mapM getPRForCommit parentSHAs
        onMergeCompletion prs isSuccess `finally` deleteBranch ciBranchName
  where
    isTry = isTryBranch ciBranchName
    checkName = if isTry then checkRunTry else checkRunMerge

    mkCIBranchUrl = do
      (repoOwner, repoName) <- getRepo
      return $ "https://github.com/" <> repoOwner <> "/" <> repoName <> "/commits/" <> ciBranchName

    onMergeCompletion prs isSuccess
      | isSuccess = do
          let prNums = map prForCommitId prs

          -- merge into base
          let invalidStagingBranch = throwIO $ InvalidStaging prNums ciBranchName
          base <- maybe invalidStagingBranch return $ fromStagingBranch ciBranchName
          updateBranch False base sha >>= \case
            Right _ -> return ()
            Left message -> throwIO $ BadUpdate sha prNums base message

          -- close PRs and delete branches
          forM_ prs $ \pr -> do
            let prNum = prForCommitId pr
                branch = prForCommitBranch pr

            -- wait until PR is marked "merged"
            whileM_ (not <$> isPRMerged prNum) $ return ()

            closePR prNum
            deleteBranch branch
            deleteBranch $ toTryBranch prNum
      | otherwise = return ()

-- | Get the configuration file for the given tree.
extractConfig :: [Int] -> Tree -> Either BotError BotConfig
extractConfig prs tree =
  case filter isConfigFile [get| tree.entries![] |] of
    [] -> Left $ ConfigFileMissing prs
    [entry] ->
      let configText = Text.encodeUtf8 [get| entry.object!.text! |]
      in first (ConfigFileInvalid prs) . decodeThrow $ configText
    _ -> error $ "Multiple '" ++ Text.unpack configFileName ++ "' files found?"
  where
    isConfigFile = (== configFileName) . [get| .name |]
