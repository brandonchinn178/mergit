{-|
Module      :  MergeBot.Handlers
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines handlers for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MergeBot.Handlers
  ( handlePullRequest
  , handleCheckSuite
  , handleCheckRun
  , handleStatus
  ) where

import Control.Monad (forM_, unless, when)
import Data.Aeson.Schema (Object, get)
import qualified GitHub.Schema.Event.CheckRun as CheckRun
import qualified GitHub.Schema.Event.CheckSuite as CheckSuite
import qualified GitHub.Schema.Event.PullRequest as PullRequest
import Servant (Handler)
import Servant.GitHub

import qualified MergeBot.Core as Core
import MergeBot.Core.Text (isTryBranch)
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
    CheckRun.REQUESTED_ACTION ->
      case [get| o.requested_action!.identifier |] of
        "lybot_run_try" -> forM_ prs $ \pr ->
          Core.startTryJob
            [get| pr.number |]
            [get| pr.head.sha |]
            [get| pr.base.sha |]
        "lybot_queue" -> Core.queuePR [get| o.check_run.id |]
        "lybot_dequeue" -> Core.dequeuePR [get| o.check_run.id |]
        _ -> return ()
    _ -> return ()
  where
    repo = [get| o.repository! |]
    prs = [get| o.check_run.pull_requests[] |]

-- | Handle the 'status' GitHub event.
handleStatus :: Object StatusEvent -> Token -> Handler ()
handleStatus o = runBotApp repo $
  -- TODO: also allow merge branches
  when (any isTryBranch [get| o.branches[].name |]) $
    Core.handleStatusUpdate True [get| o.sha |]
  where
    repo = [get| o.repository! |]
