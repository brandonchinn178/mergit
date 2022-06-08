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

{- |
Module      :  Mergit.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core Mergit functionality.
-}
module Mergit.Core (
  createCheckRuns,
  startTryJob,
  queuePR,
  dequeuePR,
  resetMerge,
  handleStatusUpdate,
  pollQueues,
) where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, void, when, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import Data.GraphQL (get)
import Data.HashMap.Strict qualified as HashMap
import Data.List (nub, partition)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Data.Yaml (decodeEither')
import Text.Printf (printf)
import UnliftIO.Exception (finally, fromEither, throwIO, withException)

import Mergit.Core.CheckRun
import Mergit.Core.Config
import Mergit.Core.Error
import Mergit.Core.GitHub
import Mergit.Core.GraphQL.Enums.CheckConclusionState qualified as CheckConclusionState
import Mergit.Core.GraphQL.Enums.CheckStatusState qualified as CheckStatusState
import Mergit.Core.GraphQL.Enums.PullRequestReviewState qualified as PullRequestReviewState
import Mergit.Core.GraphQL.Enums.StatusState qualified as StatusState
import Mergit.Core.Monad
import Mergit.Core.Status
import Mergit.Core.Text

default (Text)

createCheckRuns :: MonadMergit m => CommitSHA -> m ()
createCheckRuns sha = do
  now <- liftIO getCurrentTime
  createTryCheckRun sha $ tryJobInitData now
  createMergeCheckRun sha $ mergeJobInitData now

-- | Start a new try job.
startTryJob :: MonadMergit m => PrNum -> CommitSHA -> BranchName -> m ()
startTryJob prNum sha base = do
  checkRun <- getCheckRunForCommit sha CheckRunTry

  let setCheckRunFailed e =
        updateCheckRuns
          [(sha, checkRun)]
          CheckRunUpdates
            { isStart = True
            , isTry = True
            , checkRunStatus = CheckRunComplete False
            , checkRunBody = [summaryInitFailed e]
            }

  (`withException` setCheckRunFailed) $ do
    mergeSHA <- createCIBranch base [(prNum, sha)] tryBranch tryMessage
    refreshCheckRuns True tryBranch mergeSHA
  where
    tryBranch = toTryBranch prNum
    tryMessage = toTryMessage prNum

-- | Add a PR to the queue.
queuePR :: MonadMergit m => PrNum -> CommitSHA -> m ()
queuePR prNum sha = do
  -- TODO: lookup how many approvals are required
  reviews <- getPRReviews prNum

  unless (PullRequestReviewState.APPROVED `elem` reviews) $
    throwIO $ UnapprovedPR prNum

  checkRun <- getCheckRun prNum CheckRunMerge
  -- TOOD: batching info
  updateCheckRuns
    [(sha, checkRun)]
    CheckRunUpdates
      { isStart = True -- previous status of merge check run was "Complete: Action Required"
      , isTry = False
      , checkRunStatus = CheckRunQueued
      , checkRunBody = [mergeJobSummaryQueued]
      }

-- | Remove a PR from the queue.
dequeuePR :: MonadMergit m => PrNum -> CommitSHA -> m ()
dequeuePR = resetMerge

-- | Remove a PR from the queue.
resetMerge :: MonadMergit m => PrNum -> CommitSHA -> m ()
resetMerge _ sha = do
  now <- liftIO getCurrentTime
  -- reset check run by completely re-creating it
  createMergeCheckRun sha $ mergeJobInitData now

-- | Handle a notification that the given commit's status has been updated.
handleStatusUpdate :: MonadMergit m => BranchName -> CommitSHA -> m ()
handleStatusUpdate = refreshCheckRuns False

-- | Load all queues and start a merge run if one is not already running.
pollQueues :: MonadMergit m => m ()
pollQueues = do
  queues <- getQueues
  void $
    flip HashMap.traverseWithKey queues $ \base prs -> do
      staging <- getBranchSHA $ toStagingBranch base
      when (isNothing staging) $ startMergeJob prs base
  where
    startMergeJob prs base = do
      let (prNums, prSHAs, checkRunIds) = unzip3 prs
          prNumsAndSHAs = zip prNums prSHAs
          prSHAsAndCheckRunIds = zip prSHAs checkRunIds

      let setCheckRunFailed e =
            updateCheckRuns
              prSHAsAndCheckRunIds
              CheckRunUpdates
                { isStart = True
                , isTry = False
                , checkRunStatus = CheckRunComplete False
                , checkRunBody = [summaryInitFailed e]
                }

      (`withException` setCheckRunFailed) $ do
        let stagingBranch = toStagingBranch base
            stagingMessage = toStagingMessage base prNums
        mergeSHA <- createCIBranch base prNumsAndSHAs stagingBranch stagingMessage
        refreshCheckRuns True stagingBranch mergeSHA

{- Helpers -}

{- | Create a branch for a try or merge job.

 * Deletes the existing try or merge branch, if one exists.
 * Errors if merge conflict
 * Errors if the .mergit.yaml file is missing or invalid
-}
createCIBranch :: MonadMergit m => BranchName -> [(PrNum, CommitSHA)] -> BranchName -> Text -> m CommitSHA
createCIBranch base prs ciBranch message = do
  deleteBranch ciBranch
  deleteBranch tempBranch

  (`finally` deleteBranch tempBranch) $ do
    baseSHA <- maybe (throwIO $ MissingBaseBranch prNums base) return =<< getBranchSHA base

    -- create temp branch off base
    createBranch tempBranch baseSHA

    -- merge prs into temp branch
    forM_ prs $ \(prNum, prSHA) -> do
      treeInitial <- getBranchTree tempBranch

      success <- mergeBranches tempBranch prSHA "[ci skip] merge into temp"
      unless success $ throwIO $ MergeConflict prNums

      -- check that tree was updated
      -- https://github.com/LeapYear/mergit/issues/180#issuecomment-1097310669
      maybe (throwIO $ TreeNotUpdated prNums prNum) return <=< retry 3 $ do
        treeUpdated <- getBranchTree tempBranch
        return $
          if treeUpdated == treeInitial
            then Nothing
            else Just ()

    -- get tree for temp branch
    tree <- getBranchTree tempBranch

    -- check missing/invalid .mergit.yaml file
    void $ fromEither $ extractConfig prNums tree

    -- create a new commit that merges all the PRs at once
    mergeSHA <- createCommit message [get| tree.oid |] (baseSHA : prSHAs)

    -- forcibly update CI branch to point to new merge commit
    createBranch ciBranch mergeSHA

    return mergeSHA
  where
    tempBranch = "temp-" <> ciBranch
    (prNums, prSHAs) = unzip prs

    retry :: MonadIO m => Int -> m (Maybe a) -> m (Maybe a)
    retry n action =
      if n <= 0
        then return Nothing
        else
          action >>= \case
            Just a -> return $ Just a
            Nothing -> do
              liftIO $ threadDelay 1000000
              retry (n - 1) action

-- | Refresh the check runs for the given CI commit.
refreshCheckRuns :: MonadMergit m => Bool -> BranchName -> CommitSHA -> m ()
refreshCheckRuns isStart ciBranchName sha = do
  CICommit{..} <- getCICommit sha checkRunType
  when (null parents) $ throwIO $ CICommitMissingParents isStart ciBranchName sha

  -- since we check the config in 'createCIBranch', we know that 'extractConfig' here will not fail
  config <-
    either (error "extractConfig failed in refreshCheckRuns") return $
      extractConfig [] commitTree

  let ciStatus = getCIStatus config commitContexts

      (state, conclusion) =
        case nub $ map ((checkRunState &&& checkRunConclusion) . snd) parents of
          [x] -> x
          _ -> error $ "Expected parents' check runs to all be in same state, got: " ++ show parents

      -- Nothing = not complete, Just = complete with is successful
      isCheckRunComplete =
        case (state, conclusion) of
          -- the state we initialize try runs to
          (CheckStatusState.COMPLETED, Just CheckConclusionState.NEUTRAL) | isTry -> Nothing
          -- the state we initialize merge runs to
          (CheckStatusState.COMPLETED, Just CheckConclusionState.ACTION_REQUIRED) | not isTry -> Nothing
          (CheckStatusState.COMPLETED, Just CheckConclusionState.SUCCESS) -> Just True
          (CheckStatusState.COMPLETED, _) -> Just False
          _ -> Nothing

      -- NB: isComplete means that the merge bot should consider a check run "done"; it does not
      -- necessarily mean that CI is done. Meaning we should not clean up yet, since more CI jobs
      -- could start and fail to checkout.
      (isComplete, isSuccess) =
        case resolveCIStatus ciStatus of
          -- a merge run in a completed state should _never_ have its conclusion changed
          --
          -- we specifically want to prevent the case of someone rerunning a merge
          -- run to have it pass and try to merge. Currently, this isn't an issue,
          -- as a merge run failing means the merge bot deletes the staging branch,
          -- and GitHub will no longer set the 'branches' field in the 'status' event,
          -- so we wouldn't be refreshing the check run anymore. But we should still
          -- check this to ensure we're not relying on that assumption
          _ | not isTry, Just success <- isCheckRunComplete -> (True, success)
          StatusState.SUCCESS -> (True, True)
          StatusState.ERROR -> (True, False)
          StatusState.FAILURE -> (True, False)
          _ -> (False, False)

  ciBranchUrl <- mkCIBranchUrl
  updateCheckRuns
    parents
    CheckRunUpdates
      { isStart
      , isTry
      , checkRunStatus =
          if isComplete
            then CheckRunComplete isSuccess
            else CheckRunInProgress
      , checkRunBody =
          let message
                | not isComplete =
                  Text.pack $ printf "CI running in the [%s](%s) branch." ciBranchName ciBranchUrl
                | otherwise =
                  case (isTry, isSuccess) of
                    (True, False) -> tryJobSummaryFailed
                    (True, True) -> tryJobSummarySuccess
                    (False, False) -> mergeJobSummaryFailed
                    (False, True) -> mergeJobSummarySuccess
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
        prs <- mapM getPRById prsFromMessage

        allPRsMerged <- areAllPRsMerged prs

        if allPRsMerged
          then do
            -- If all PRs are merged, then this status was from a post-merge, non-blocking
            -- CI job. Don't attempt to merge PRs / delete branches again.
            return ()
          else do
            -- If all of the PRs are still open, run the merge post-run actions and delete the
            -- staging branch when finished, even if the merge run failed (so that the next
            -- merge run can start).
            onMergeCompletion parents prs isSuccess `finally` deleteBranch ciBranchName
  where
    isTry = isTryBranch ciBranchName
    checkRunType = if isTry then CheckRunTry else CheckRunMerge

    mkCIBranchUrl = do
      (repoOwner, repoName) <- getRepo
      let url = printf "https://github.com/%s/%s/commits/%s" repoOwner repoName ciBranchName
      return (url :: String)

    areAllPRsMerged prs = do
      let (mergedPRs, nonMergedPRs) = partition prIsMerged prs
          getIds = map prId

      case (mergedPRs, nonMergedPRs) of
        (_, []) -> return True
        ([], _) -> return False
        -- If there are some PRs merged and some not, then the merge bot is in a really bad state.
        (_, _) -> throwIO $ SomePRsMerged (getIds mergedPRs) (getIds nonMergedPRs)

    onMergeCompletion parents prs isSuccess
      | isSuccess = do
        let prNums = map prId prs

        -- check if any PRs were updated underneath us
        -- https://github.com/LeapYear/mergit/issues/126
        let parentSHAs = map fst parents
        case filter (\pr -> prSHA pr `notElem` parentSHAs) prs of
          [] -> return ()
          updatedPRs -> do
            let prSHAs = map prSHA prs
                unaccountedSHAs = filter (`notElem` prSHAs) parentSHAs
            throwIO $ PRWasUpdatedDuringMergeRun prNums (map prId updatedPRs) unaccountedSHAs

        -- merge into base
        let invalidStagingBranch = throwIO $ InvalidStaging prNums ciBranchName
        base <- maybe invalidStagingBranch return $ fromStagingBranch ciBranchName
        updateBranch False base sha >>= \case
          Right _ -> return ()
          Left message -> throwIO $ BadUpdate sha prNums base message

        -- close PRs and delete branches
        forM_ prs $ \pr -> do
          let prNum = prId pr
              branch = prBranch pr

          -- wait until PR is marked "merged"
          let waitUntilPRIsMerged i = do
                prIsMerged <- isPRMerged prNum
                unless prIsMerged $ do
                  -- sleep for 1 second, try 5 times
                  liftIO $ threadDelay 1000000
                  if i < 5
                    then waitUntilPRIsMerged $ i + 1
                    else throwIO $ BadUpdate sha prNums base $ Text.pack $ printf "PR did not merge: %d" prNum
          waitUntilPRIsMerged (0 :: Int)

          closePR prNum
          deleteBranch branch
          deleteBranch $ toTryBranch prNum
      | otherwise = return ()

-- | Get the configuration file for the given tree.
extractConfig :: [PrNum] -> Tree -> Either MergitError MergitConfig
extractConfig prs tree =
  case filter isConfigFile [get| tree.entries![] |] of
    [] -> Left $ ConfigFileMissing prs
    [entry] ->
      let configText = [get| entry.object!.__fragment!.text! |]
       in first (ConfigFileInvalid prs . Text.pack . show) . decodeEither' . Text.encodeUtf8 $ configText
    _ -> error $ "Multiple '" ++ Text.unpack configFileName ++ "' files found?"
  where
    isConfigFile = (== configFileName) . [get| .name |]
