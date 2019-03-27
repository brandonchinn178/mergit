{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the MergeBot GitHub application.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (forkFinally, myThreadId, threadDelay, throwTo)
import Control.Monad (forever, void)
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
    )

server :: Server MergeBotApp
server
  =    handlePullRequest
  :<|> handleCheckSuite
  :<|> handleCheckRun
  :<|> handleStatus

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  return $ serveWithContext (Proxy @MergeBotApp) (params :. EmptyContext) server

pollQueues :: IO ()
pollQueues = forever $ do
  void $ runBotAppForAllInstalls Core.pollQueues
  threadDelay tenMinutes
  where
    tenMinutes = 10 * 60e6

runMergeBot :: IO ()
runMergeBot = do
  threadId <- myThreadId
  void $ pollQueues `forkFinally` either (throwTo threadId) return
  run 3000 =<< initApp
