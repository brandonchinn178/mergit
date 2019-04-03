{-|
Module      :  MergeBot.Handlers
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines handlers for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MergeBot.Handlers
  ( handlePullRequest
  , handleCheckSuite
  , handleCheckRun
  , handleStatus
  , handlePush
  ) where

import Control.Monad (unless, when)
import Control.Monad.Logger (logInfoN)
import Data.Aeson.Schema (Object, get)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (unOID)
import qualified GitHub.Schema.Event.CheckRun as CheckRun
import qualified GitHub.Schema.Event.CheckSuite as CheckSuite
import qualified GitHub.Schema.Event.PullRequest as PullRequest
import Servant (Handler)
import Servant.GitHub

import qualified MergeBot.Core as Core
import MergeBot.Core.Actions (MergeBotAction(..), parseAction)
import MergeBot.Core.Error (BotError(..), throwIO)
import qualified MergeBot.Core.GitHub as Core
import MergeBot.Core.Text (isStagingBranch, isTryBranch)
import MergeBot.Monad (runBotApp)

-- | Handle the 'pull_request' GitHub event.
handlePullRequest :: Object PullRequestEvent -> Token -> Handler ()
handlePullRequest o = runBotApp repo $
  case [get| o.action |] of
    PullRequest.OPENED -> do
      logInfoN $ "PR created: " <> Text.pack (show [get| o.pull_request.number |])
      Core.createCheckRuns [get| o.pull_request.head.sha |]
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'check_suite' GitHub event.
handleCheckSuite :: Object CheckSuiteEvent -> Token -> Handler ()
handleCheckSuite o = runBotApp repo $
  case [get| o.action |] of
    CheckSuite.REQUESTED ->
      -- create check runs for any commits pushed to a PR
      unless (null [get| o.check_suite.pull_requests |]) $
        Core.createCheckRuns [get| o.check_suite.head_sha |]
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'check_run' GitHub event.
handleCheckRun :: Object CheckRunEvent -> Token -> Handler ()
handleCheckRun o = runBotApp repo $
  case [get| o.action |] of
    CheckRun.REQUESTED_ACTION -> do
      pr <- case [get| o.check_run.pull_requests[] |] of
        [pr'] -> return pr'
        _ -> throwIO $ NotOnePRInCheckRun o

      let prNum = [get| pr.number |]
          prNum' = Text.pack $ show prNum
          sha = [get| o.check_run.head_sha |]
          action = [get| o.requested_action!.identifier |]

      unless (sha == [get| pr.head.sha |]) $ do
        logInfoN $ "Received action `" <> action <> "` for commit `" <> unOID sha <> "` on PR #" <> prNum'
        throwIO $ CommitNotPRHead prNum sha

      case parseAction action of
        Just BotTry -> do
          logInfoN $ "Trying PR #" <> prNum' <> " for commit: " <> unOID sha
          Core.startTryJob prNum sha [get| pr.base.ref |]
        Just BotQueue -> do
          logInfoN $ "Queuing PR #" <> prNum'
          Core.queuePR prNum
        Just BotDequeue -> do
          logInfoN $ "Dequeuing PR #" <> prNum'
          Core.dequeuePR prNum
        Just BotResetMerge -> do
          logInfoN $ "Resetting merge check run for PR #" <> prNum'
          Core.resetMerge prNum
        Nothing -> return ()
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'status' GitHub event.
handleStatus :: Object StatusEvent -> Token -> Handler ()
handleStatus o = runBotApp repo $
  case [get| o.branches[].name |] of
    [branch] ->
      let handleStatus' isTry = Core.handleStatusUpdate isTry branch [get| o.sha |]
      in if
        | isTryBranch branch -> handleStatus' True
        | isStagingBranch branch -> handleStatus' False
        | otherwise -> return ()
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'push' GitHub event.
handlePush :: Object PushEvent -> Token -> Handler ()
handlePush o = runBotApp repo $
  when (isCreated && isCIBranch && not isBot) $ do
    Core.deleteBranch branch
    throwIO $ CIBranchPushed o
  where
    repo = [get| o.repository! |]
    isCreated = [get| o.created |]
    isCIBranch = isStagingBranch branch || isTryBranch branch
    isBot = [get| o.sender.type |] == "Bot"
    branch = case Text.splitOn "/" [get| o.ref |] of
      [] -> error $ "Bad ref: " ++ Text.unpack [get| o.ref |]
      l -> last l
