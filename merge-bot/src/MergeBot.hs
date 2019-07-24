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
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException)
import Control.Monad (forever, (>=>))
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Servant.GitHub
import UnliftIO.Async (concurrently_, waitCatch, withAsync)

import MergeBot.Auth (AuthParams(..), loadAuthParams)
import qualified MergeBot.Core as Core
import MergeBot.Monad (runBaseApp, runBaseHandler, runBotAppForAllInstalls)
import MergeBot.Routes (MergeBotRoutes, handleMergeBotRoutes)

type BaseAppContext = '[CookieSettings, JWTSettings, GitHubAppParams]

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  authParams <- loadAuthParams

  let context :: Context BaseAppContext
      context = cookieSettings authParams :. jwtSettings authParams :. params :. EmptyContext

  return $ serveWithContext (Proxy @MergeBotRoutes) context $
    hoistServerWithContext (Proxy @MergeBotRoutes) (Proxy @BaseAppContext) (runBaseHandler params authParams)
      handleMergeBotRoutes

pollQueues :: IO ()
pollQueues = do
  params <- loadGitHubAppParams
  authParams <- loadAuthParams

  withAsync (runBaseApp params authParams $ runBotAppForAllInstalls Core.pollQueues) $ waitCatch >=> \case
    Right _ -> return ()
    Left e -> putStrLn $ displayException e

  -- wait 10 minutes
  threadDelay $ 10 * 60e6

runMergeBot :: IO ()
runMergeBot = concurrently_ (forever pollQueues) (run 3000 =<< initApp)
