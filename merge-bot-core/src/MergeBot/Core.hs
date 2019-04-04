{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
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
import GitHub.REST (KeyValue(..))
import UnliftIO.Exception (finally, fromEither, throwIO)

import MergeBot.Core.Actions
import MergeBot.Core.Config
import MergeBot.Core.Error
import MergeBot.Core.GitHub
import MergeBot.Core.Monad
import MergeBot.Core.Text

default (Text)

createCheckRuns :: MonadMergeBot m => GitObjectID -> m ()
createCheckRuns sha = createTryCheckRun sha >> createMergeCheckRun sha

-- | Create the check run for trying PRs.
createTryCheckRun :: MonadMergeBot m => GitObjectID -> m ()
createTryCheckRun sha = do
  now <- liftIO getCurrentTime
  createCheckRun
    [ "name"         := checkRunTry
    , "head_sha"     := sha
    , "status"       := "completed"
    , "conclusion"   := "neutral"
    , "completed_at" := now
    , "output"       := output tryJobLabelInit tryJobSummaryInit
    , "actions"      := [renderAction BotTry]
    ]

-- | Create the check run for queuing/merging PRs.
createMergeCheckRun :: MonadMergeBot m => GitObjectID -> m ()
createMergeCheckRun sha = do
  now <- liftIO getCurrentTime
  createCheckRun $
    [ "name"         := checkRunMerge
    , "head_sha"     := sha
    ] ++ mergeJobInitData now

-- | Start a new try job.
startTryJob :: MonadMergeBot m => Int -> GitObjectID -> Text -> m ()
startTryJob prNum prSHA base = do
  mergeSHA <- createCIBranch base [prSHA] tryBranch tryMessage

  refreshCheckRuns True True tryBranch mergeSHA
  where
    tryBranch = toTryBranch prNum
    tryMessage = toTryMessage prNum

-- | Add a PR to the queue.
queuePR :: MonadMergeBot m => Int -> m ()
queuePR prNum = do
  -- TODO: lookup how many approvals are required
  reviews <- getPRReviews prNum

  unless (PullRequestReviewState.APPROVED `elem` reviews) $
    throwIO $ UnapprovedPR prNum

  checkRunId <- getCheckRun prNum checkRunMerge
  -- TOOD: batching info
  updateCheckRun checkRunId
    [ "status"  := "queued"
    , "output"  := output mergeJobLabelQueued mergeJobSummaryQueued
    , "actions" := [renderAction BotDequeue]
    ]

-- | Remove a PR from the queue.
dequeuePR :: MonadMergeBot m => Int -> m ()
dequeuePR = resetMerge

-- | Remove a PR from the queue.
resetMerge :: MonadMergeBot m => Int -> m ()
resetMerge prNum = do
  checkRunId <- getCheckRun prNum checkRunMerge
  now <- liftIO getCurrentTime
  updateCheckRun checkRunId $ mergeJobInitData now

-- | Handle a notification that the given commit's status has been updated.
handleStatusUpdate :: MonadMergeBot m => Bool -> Text -> GitObjectID -> m ()
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
      let (prNums, prSHAs) = unzip prs
          stagingBranch = toStagingBranch base
          stagingMessage = toStagingMessage base prNums
      mergeSHA <- createCIBranch base prSHAs stagingBranch stagingMessage

      refreshCheckRuns True False stagingBranch mergeSHA

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
    prNums <- mapM (fmap fst . getPRForCommit) prSHAs

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

-- | Update the check runs for the given CI commit, including any additional data provided.
refreshCheckRuns :: MonadMergeBot m => Bool -> Bool -> Text -> GitObjectID -> m ()
refreshCheckRuns isStart isTry ciBranchName sha = do
  CICommit{..} <- getCICommit sha checkName
  let (parentSHAs, checkRuns) = unzip parents

  -- since we check the config in 'createCIBranch', we know that 'extractConfig' here will not fail
  config <-
    either (error "extractConfig failed in refreshCheckRuns") return $
      extractConfig [] commitTree

  now <- liftIO getCurrentTime
  (repoOwner, repoName) <- getRepo

  let checkRunState = StatusState.summarize $ map [get| .state |] commitContexts
      (isComplete, isSuccess) = case checkRunState of
        StatusState.SUCCESS -> (True, True)
        StatusState.ERROR -> (True, False)
        StatusState.FAILURE -> (True, False)
        _ -> (False, False)

      checkRunData = concat
        [ if isStart then [ "started_at" := now ] else []
        , if isComplete
            then
              [ "status"       := "completed"
              , "conclusion"   := if isSuccess then "success" else "failure"
              , "completed_at" := now
              ]
            else
              [ "status" := "in_progress"
              ]
        , let doneActions
                | isComplete && isTry = [renderAction BotTry]
                | isComplete && not isSuccess = [renderAction BotQueue]
                | isComplete && not isTry && isSuccess = [renderAction BotResetMerge]
                | otherwise = []
          in [ "actions" := doneActions ]
        , let repoUrl = "https://github.com/" <> repoOwner <> "/" <> repoName
              ciBranchUrl = repoUrl <> "/commits/" <> ciBranchName
              ciInfo = "CI running in the [" <> ciBranchName <> "](" <> ciBranchUrl <> ") branch."
              ciStatus = displayCIStatus config commitContexts
              jobLabel = case (isComplete, isTry) of
                (False, True) -> tryJobLabelRunning
                (False, False) -> mergeJobLabelRunning
                (True, True) -> tryJobLabelDone
                (True, False) -> mergeJobLabelDone
              jobSummary
                | not isComplete = [ciInfo, ciStatus]
                | isTry = [tryJobSummaryDone, ciStatus]
                | not isSuccess = [mergeJobSummaryFailed, ciStatus]
                | otherwise = [mergeJobSummarySuccess, ciStatus]
          in [ "output" := output jobLabel (unlines2 jobSummary) ]
        ]

  mapM_ (`updateCheckRun` checkRunData) checkRuns

  when isComplete $
    -- if complete, make sure the CI branch is deleted at the end
    (`finally` deleteBranch ciBranchName) $
      -- if successful merge run, merge into base
      when (isSuccess && not isTry) $ do
        -- get pr information for parent commits
        prs <- mapM getPRForCommit parentSHAs
        let prNums = map fst prs

        -- merge into base
        let invalidStagingBranch = throwIO $ InvalidStaging prNums ciBranchName
        base <- maybe invalidStagingBranch return $ fromStagingBranch ciBranchName
        success <- updateBranch False base sha
        unless success $ throwIO $ NotFastForward prNums base

        -- close PRs and delete branches
        forM_ prs $ \(prNum, branch) -> do
          -- wait until PR is marked "merged"
          whileM_ (not <$> isPRMerged prNum) $ return ()

          closePR prNum
          deleteBranch branch
  where
    checkName = if isTry then checkRunTry else checkRunMerge
    unlines2 = Text.concat . map (<> "\n\n")

-- | Get text containing Markdown showing a list of jobs required by the merge bot and their status
-- in CI.
displayCIStatus :: BotConfig -> [CIContext] -> Text
displayCIStatus BotConfig{requiredStatuses} contexts =
  let statuses = foldl updateStatusMap (mkStatusMap requiredStatuses) contexts
  in Text.unlines $ header ++ fromStatusMap statuses
  where
    header =
      [ "CI Job | Status"
      , ":-----:|:-----:"
      ]
    mkStatusMap = HashMap.fromList . map (, (StatusState.EXPECTED, Nothing))
    updateStatusMap statuses context = HashMap.adjust
      (const [get| context.(state, targetUrl) |])
      [get| context.context |]
      statuses
    fromStatusMap statuses =
      -- iterate on requiredStatuses to keep order
      map (\c -> mkLine c $ statuses HashMap.! c) requiredStatuses
    mkLine context (state, url) =
      let emoji = case state of
            StatusState.ERROR    -> "❗"
            StatusState.EXPECTED -> "💤"
            StatusState.FAILURE  -> "❌"
            StatusState.PENDING  -> "⏳"
            StatusState.SUCCESS  -> "✅"
          link = case url of
            Nothing -> context
            Just url' -> "[" <> context <> "](" <> url' <> ")"
      in link <> " | " <> emoji

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
