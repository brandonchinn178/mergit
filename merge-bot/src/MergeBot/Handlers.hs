{-|
Module      :  MergeBot.Handlers
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines handlers for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module MergeBot.Handlers
  ( handleCheckSuite
  , handleCheckRun
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Schema (Object, get)
import Data.Text (Text)
import GitHub.REST
import Servant
import qualified Servant.GitHub as GitHub

import MergeBot.Core (startTryJob)
import MergeBot.Monad

default (Text)

-- | Handle the 'check_suite' GitHub event.
handleCheckSuite :: Object GitHub.CheckSuiteSchema -> GitHub.Token -> Handler ()
handleCheckSuite o = runGitHub $
  case [get| o.action |] of
    GitHub.CheckSuiteRequestedAction -> do
      createCheckRun repo
        [ "name"     := "Bot Try"
        , "head_sha" := sha
        , "output" :=
          [ "title"   := "Try Run"
          , "summary" := "No try run available. Click \"Run Try\" above to begin your try run."
          ]
        , "actions" :=
          [ [ "label"       := "Run Try"
            , "description" := "Start a try run"
            , "identifier"  := "lybot_run_try"
            ]
          ]
        ]
      createCheckRun repo
        [ "name"     := "Bot Merge"
        , "head_sha" := sha
        , "output" :=
          [ "title"   := "Merge Run"
          , "summary" := "Not queued. Click \"Queue\" above to queue this PR for the next merge run."
          ]
        , "actions" :=
          [ [ "label"       := "Queue"
            , "description" := "Queue this PR"
            , "identifier"  := "lybot_queue"
            ]
          ]
        ]
    _ -> return ()
  where
    repo = [get| o.repository!.full_name |]
    sha = [get| o.check_suite.head_sha |]

-- | Handle the 'check_run' GitHub event.
handleCheckRun :: Object GitHub.CheckRunSchema -> GitHub.Token -> Handler ()
handleCheckRun o = runGitHub $
  case [get| o.action |] of
    GitHub.CheckRunRequestedAction ->
      case [get| o.requested_action!.identifier |] of
        "lybot_run_try" -> mapM_ startTryJob [get| o.check_run.pull_requests[].number |]
        "lybot_queue" -> liftIO $ putStrLn "Queue PR"
        _ -> return ()
    _ -> return ()
