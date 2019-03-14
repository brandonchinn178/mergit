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
  ( handleCheckSuite
  , handleCheckRun
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Schema (Object, get)
import qualified GitHub.Schema.Event.CheckRun as CheckRun
import qualified GitHub.Schema.Event.CheckSuite as CheckSuite
import Servant (Handler)
import Servant.GitHub

import MergeBot.Core (createMergeCheckRun, createTryCheckRun, startTryJob)
import MergeBot.Monad (runGitHub)

-- | Handle the 'check_suite' GitHub event.
handleCheckSuite :: Object CheckSuiteEvent -> Token -> Handler ()
handleCheckSuite o = runGitHub repo $
  case [get| o.action |] of
    CheckSuite.REQUESTED -> do
      createTryCheckRun sha
      createMergeCheckRun sha
    _ -> return ()
  where
    repo = [get| o.repository!.full_name |]
    sha = [get| o.check_suite.head_sha |]

-- | Handle the 'check_run' GitHub event.
handleCheckRun :: Object CheckRunEvent -> Token -> Handler ()
handleCheckRun o = runGitHub repo $
  case [get| o.action |] of
    CheckRun.REQUESTED_ACTION ->
      case [get| o.requested_action!.identifier |] of
        "lybot_run_try" -> mapM_ (uncurry3 startTryJob) prs
        "lybot_queue" -> liftIO $ putStrLn "Queue PR"
        _ -> return ()
    _ -> return ()
  where
    repo = [get| o.repository!.full_name |]
    prs = [get| o.check_run.pull_requests[].(number, head.sha, base.sha) |]
    uncurry3 f (a,b,c) = f a b c
