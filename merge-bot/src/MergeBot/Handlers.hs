{-|
Module      :  MergeBot.Handlers
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines handlers for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module MergeBot.Handlers
  ( handlePullRequest
  , handleCheckSuite
  , handleCheckRun
  , handleStatus
  ) where

import Control.Monad (unless)
import Data.Aeson.Schema (Object, get)
import GitHub.Data.GitObjectID (unOID')
import qualified GitHub.Schema.Event.CheckRun as CheckRun
import qualified GitHub.Schema.Event.CheckSuite as CheckSuite
import qualified GitHub.Schema.Event.PullRequest as PullRequest
import Servant (Handler)
import Servant.GitHub

import qualified MergeBot.Core as Core
import MergeBot.Core.Actions (MergeBotAction(..), parseAction)
import MergeBot.Core.Text (isStagingBranch, isTryBranch)
import MergeBot.Monad (runBotApp)

-- | Handle the 'pull_request' GitHub event.
handlePullRequest :: Object PullRequestEvent -> Token -> Handler ()
handlePullRequest o = runBotApp repo $
  case [get| o.action |] of
    PullRequest.OPENED -> Core.createCheckRuns [get| o.pull_request.head.sha |]
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'check_suite' GitHub event.
handleCheckSuite :: Object CheckSuiteEvent -> Token -> Handler ()
handleCheckSuite o = runBotApp repo $
  case [get| o.action |] of
    CheckSuite.REQUESTED ->
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
      let sha = [get| o.check_run.head_sha |]
      unless (sha == [get| pr.head.sha |]) $
        -- TODO: better error handling
        fail $ "Commit '" ++ unOID' sha ++ "' is not HEAD for PR #" ++ show prNum

      case parseAction [get| o.requested_action!.identifier |] of
        Just BotTry ->
          Core.startTryJob
            prNum
            [get| pr.head.sha |]
            [get| pr.base.ref |]
        Just BotQueue -> Core.queuePR prNum
        Just BotDequeue -> Core.dequeuePR prNum
        Nothing -> return ()
    _ -> return ()
  where
    repo = [get| o.repository! |]
    pr = case [get| o.check_run.pull_requests[] |] of
      [] -> error $ "No PRs found in check run: " ++ show o
      [pr'] -> pr'
      _ -> error $ "Multiple PRs found for check run: " ++ show o
    prNum = [get| pr.number |]

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
