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

initApp :: GitHubAppParams -> AuthParams -> Application
initApp ghAppParams authParams =
  serveWithContext (Proxy @MergeBotRoutes) context $
    hoistServerWithContext (Proxy @MergeBotRoutes) (Proxy @BaseAppContext) runBaseHandler'
      handleMergeBotRoutes
  where
    context :: Context BaseAppContext
    context = cookieSettings authParams :. jwtSettings authParams :. ghAppParams :. EmptyContext

    runBaseHandler' = runBaseHandler ghAppParams authParams

pollQueues :: GitHubAppParams -> AuthParams -> IO ()
pollQueues ghAppParams authParams = do
  runAsync $ runBaseApp ghAppParams authParams $ runBotAppForAllInstalls Core.pollQueues

  -- wait 10 minutes
  threadDelay $ 10 * 60e6
  where
    -- | Run the given action asynchronously, printing any exceptions thrown
    runAsync action = withAsync action $ waitCatch >=> \case
      Right _ -> return ()
      Left e -> putStrLn $ displayException e

runMergeBot :: IO ()
runMergeBot = do
  ghAppParams <- loadGitHubAppParams
  authParams <- loadAuthParams

  concurrently_
    (forever $ pollQueues ghAppParams authParams)
    (run 3000 $ initApp ghAppParams authParams)
