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
import Crypto.JWT (fromRSA)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.X509 (PrivKey(..))
import Data.X509.File (readKeyFile)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server (CookieSettings(..), defaultCookieSettings, defaultJWTSettings)
import Servant.GitHub
import System.Environment (lookupEnv)
import UnliftIO.Async (concurrently_, waitCatch, withAsync)

import qualified MergeBot.Core as Core
import MergeBot.Monad (runBotAppForAllInstalls)
import MergeBot.Routes (MergeBotRoutes, handleMergeBotRoutes)

initApp :: IO Application
initApp = do
  params <- loadGitHubAppParams

  jwkFile <- fromMaybe "conf/cookie-jwk.pem" <$> lookupEnv "COOKIE_JWK"
  jwk <- readKeyFile jwkFile >>= \case
    (PrivKeyRSA key):_ -> return $ fromRSA key
    _ -> fail $ "RSA key not found in key file: " ++ jwkFile

  let cookieSettings = defaultCookieSettings
        { cookieIsSecure = NotSecure
        , sessionCookieName = "merge-bot-github-token"
        }
      jwtSettings = defaultJWTSettings jwk
      context = cookieSettings :. jwtSettings :. params :. EmptyContext

  return $ serveWithContext (Proxy @MergeBotRoutes) context handleMergeBotRoutes

pollQueues :: IO ()
pollQueues = do
  withAsync (runBotAppForAllInstalls Core.pollQueues) $ waitCatch >=> \case
    Right _ -> return ()
    Left e -> putStrLn $ displayException e

  -- wait 10 minutes
  threadDelay $ 10 * 60e6

runMergeBot :: IO ()
runMergeBot = concurrently_ (forever pollQueues) (run 3000 =<< initApp)
