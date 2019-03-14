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
  ) where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Schema (Object, get)
import qualified GitHub.Schema.Event.CheckRun as CheckRun
import qualified GitHub.Schema.Event.CheckSuite as CheckSuite
import qualified GitHub.Schema.Event.PullRequest as PullRequest
import Servant (Handler)
import Servant.GitHub

import MergeBot.Core (createCheckRuns, startTryJob)
import MergeBot.Monad (runBotApp)

-- | Handle the 'pull_request' GitHub event.
handlePullRequest :: Object PullRequestEvent -> Token -> Handler ()
handlePullRequest o = runBotApp repo $
  case [get| o.action |] of
    PullRequest.OPENED -> createCheckRuns [get| o.pull_request.head.sha |]
    _ -> return ()
  where
    repo = [get| o.repository! |]

-- | Handle the 'check_suite' GitHub event.
handleCheckSuite :: Object CheckSuiteEvent -> Token -> Handler ()
handleCheckSuite o = runBotApp repo $
  case [get| o.action |] of
    CheckSuite.REQUESTED ->
      unless (null [get| o.check_suite.pull_requests |]) $
        createCheckRuns [get| o.check_suite.head_sha |]
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
          startTryJob
            [get| o.check_run.id |]
            [get| pr.number |]
            [get| pr.head.sha |]
            [get| pr.base.sha |]
        "lybot_queue" -> liftIO $ putStrLn "Queue PR"
        _ -> return ()
    _ -> return ()
  where
    repo = [get| o.repository! |]
    prs = [get| o.check_run.pull_requests[] |]
