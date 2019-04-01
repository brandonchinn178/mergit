{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the MergeBot GitHub application.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, waitCatch, withAsync)
import Control.Exception (displayException)
import Control.Monad (forever, (>=>))
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub

import qualified MergeBot.Core as Core
import MergeBot.Handlers
import MergeBot.Monad (runBotAppForAllInstalls)

type MergeBotApp
  = "webhook" :>
    (    GitHubEvent 'PullRequestEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'CheckSuiteEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'CheckRunEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'StatusEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'PushEvent :> WithToken :> GitHubAction
    )

server :: Server MergeBotApp
server
  =    handlePullRequest
  :<|> handleCheckSuite
  :<|> handleCheckRun
  :<|> handleStatus
  :<|> handlePush

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  return $ serveWithContext (Proxy @MergeBotApp) (params :. EmptyContext) server

pollQueues :: IO ()
pollQueues = do
  withAsync (runBotAppForAllInstalls Core.pollQueues) $ waitCatch >=> \case
    Right _ -> return ()
    -- TODO: better logging
    Left e -> putStrLn $ displayException e

  -- wait 10 minutes
  threadDelay $ 10 * 60e6

runMergeBot :: IO ()
runMergeBot = concurrently_ (forever pollQueues) (run 3000 =<< initApp)
