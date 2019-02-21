{-|
Module      :  MergeBot.Server
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the merge bot server running as a GitHub App.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Server (initApp) where

import Control.Concurrent.MVar (newMVar)
import qualified Data.ByteString.Char8 as Char8
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import System.Environment (getEnv)
import Text.Read (readMaybe)

import MergeBot.Core.State (newBotState)
import MergeBot.Server.Monad
import MergeBot.Server.Payload
import MergeBot.Server.Security

initApp :: IO Application
initApp = do
  ghAppId <- parseInt =<< getEnv "GITHUB_APP_ID"
  ghWebhookSecret <- Char8.pack <$> getEnv "GITHUB_WEBHOOK_SECRET"
  signer <- loadSigner =<< getEnv "PRIVATE_KEY_FILE"

  botState <- newMVar newBotState -- TODO: this should go away

  return $ \request respond -> do
    GitHubPayload{..} <- parsePayload request

    -- verify request and get access token
    checkSignature ghWebhookSecret payloadBody ghSignature
    ghToken <- getToken signer ghAppId installationId

    -- run action and respond with 200
    runHandler AppState{..} eventHandler
    respond $ responseLBS status200 [] ""

{- Helpers -}

parseInt :: Monad m => String -> m Int
parseInt x = maybe (fail $ "Invalid int: " ++ x) return $ readMaybe x
