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
import Control.Exception (displayException)
import Control.Monad (forever, (>=>))
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.GitHub
import UnliftIO.Async (concurrently_, waitCatch, withAsync)

import qualified MergeBot.Core as Core
import MergeBot.Monad (runBotAppForAllInstalls)
import MergeBot.Routes.Debug (DebugRoutes, handleDebugRoutes)
import MergeBot.Routes.Webhook (WebhookRoutes, handleWebhookRoutes)

type MergeBotApp =
  "webhook" :> WebhookRoutes
  :<|> DebugRoutes

server :: Server MergeBotApp
server = handleWebhookRoutes :<|> handleDebugRoutes

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams
  return $ serveWithContext (Proxy @MergeBotApp) (params :. EmptyContext) server

pollQueues :: IO ()
pollQueues = do
  withAsync (runBotAppForAllInstalls Core.pollQueues) $ waitCatch >=> \case
    Right _ -> return ()
    Left e -> putStrLn $ displayException e

  -- wait 10 minutes
  threadDelay $ 10 * 60e6

runMergeBot :: IO ()
runMergeBot = concurrently_ (forever pollQueues) (run 3000 =<< initApp)
