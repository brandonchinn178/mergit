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

module MergeBot (runMergeBot) where

import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub

import MergeBot.Handlers

type MergeBotApp
  = "webhook" :> GitHubSigned :>
    (    GitHubEvent 'InstallationEvent :> WithToken :> GitHubAction
    :<|> GitHubEvent 'PullRequestEvent :> WithToken :> GitHubAction
    )

server :: Server MergeBotApp
server
  =    handleInstallation
  :<|> handlePullRequest

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  return $ serveWithContext (Proxy @MergeBotApp) (params :. EmptyContext) server

runMergeBot :: IO ()
runMergeBot = run 3000 =<< initApp
