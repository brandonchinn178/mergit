{-|
Module      :  MergeBot
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the entrypoint for the MergeBot GitHub application.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=400 #-}

module MergeBot (runMergeBot) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException)
import Control.Monad (forever, (>=>))
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub
import UnliftIO.Async (concurrently_, waitCatch, withAsync)

import qualified MergeBot.Core as Core
import MergeBot.Monad (runBotAppForAllInstalls)
import MergeBot.Routes (MergeBotRoutes, handleMergeBotRoutes)
import MergeBot.Routes.Auth (AuthParams(..), loadAuthParams)

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  authParams <- loadAuthParams

  let context = cookieSettings authParams :. jwtSettings authParams :. params :. EmptyContext

  return $ serveWithContext (Proxy @MergeBotRoutes) context $ handleMergeBotRoutes authParams

pollQueues :: IO ()
pollQueues = do
  withAsync (runBotAppForAllInstalls Core.pollQueues) $ waitCatch >=> \case
    Right _ -> return ()
    Left e -> putStrLn $ displayException e

  -- wait 10 minutes
  threadDelay $ 10 * 60e6

runMergeBot :: IO ()
runMergeBot = concurrently_ (forever pollQueues) (run 3000 =<< initApp)
