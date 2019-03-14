{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the MergeBot GitHub application.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub

import MergeBot.Handlers

type MergeBotApp
  = "webhook" :>
    (    GitHubEvent 'PullRequestEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'CheckSuiteEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'CheckRunEvent :> WithToken :> GitHubAction
    )

server :: Server MergeBotApp
server
  =    handlePullRequest
  :<|> handleCheckSuite
  :<|> handleCheckRun

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  return $ serveWithContext (Proxy @MergeBotApp) (params :. EmptyContext) server

runMergeBot :: IO ()
runMergeBot = run 3000 =<< initApp
