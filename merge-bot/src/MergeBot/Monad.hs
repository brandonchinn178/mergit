{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for running GitHubT actions.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Monad
  ( runBotApp
  , runBotAppForAllInstalls
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Schema (Object, get, schema)
import Data.Text (Text)
import GitHub.REST
    (GHEndpoint(..), GitHubState(..), Token, queryGitHub, runGitHubT)
import GitHub.REST.Auth (getJWTToken)
import GitHub.Schema.Repository (RepoWebhook)
import Network.HTTP.Types (StdMethod(..))
import Servant (Handler, runHandler)
import Servant.GitHub (GitHubAppParams(..), loadGitHubAppParams)
import Servant.GitHub.Security (getToken)
import UnliftIO.Exception (throwIO)

import MergeBot.Core.Monad (BotAppT, BotSettings(..), parseRepo, runBotAppT)

type BotApp = BotAppT IO

-- | A helper around 'runBotAppT' for easy use by the Servant handlers.
runBotApp :: Object RepoWebhook -> BotApp a -> Token -> Handler a
runBotApp = runBotApp' . [get| .full_name |]

-- | A helper around 'runBotAppT'.
runBotApp' :: Text -> BotApp a -> Token -> Handler a
runBotApp' repo action token = liftIO $ do
  GitHubAppParams{ghUserAgent, ghAppId} <- loadGitHubAppParams
  (`runBotAppT` action) BotSettings
    { userAgent = ghUserAgent
    , appId = ghAppId
    , ..
    }
  where
    (repoOwner, repoName) = parseRepo repo

-- | A helper that runs the given action for every repository that the merge bot is installed on.
--
-- Returns a list of the results, along with the repository that produced each result.
runBotAppForAllInstalls :: BotApp a -> IO [(Text, a)]
runBotAppForAllInstalls action = do
  GitHubAppParams{ghUserAgent, ghAppId, ghSigner} <- loadGitHubAppParams

  -- get all installations
  jwtToken <- getJWTToken ghSigner ghAppId
  let mkState token = GitHubState
        { token
        , userAgent = ghUserAgent
        , apiVersion = "machine-man-preview"
        }
      jwtState = mkState jwtToken
  installations <- map [get| .id |] <$> runGitHubT jwtState getInstallations

  -- run the given action in each repo
  fmap concat $ forM installations $ \installationId -> do
    installToken <- getToken ghSigner ghAppId ghUserAgent installationId
    let state = mkState installToken
    repositories <- [get| .repositories[].full_name |] <$> runGitHubT state getRepositories
    forM repositories $ \repo -> do
      result <- runHandler $ runBotApp' repo action installToken
      either throwIO (return . (repo,)) result
  where
    getInstallations = queryGitHub @_ @[Object [schema| { id: Int } |]] GHEndpoint
      { method = GET
      , endpoint = "/app/installations"
      , endpointVals = []
      , ghData = []
      }
    getRepositories = queryGitHub @_ @(Object [schema| { repositories: List { full_name: Text } } |]) GHEndpoint
      { method = GET
      , endpoint = "/installation/repositories"
      , endpointVals = []
      , ghData = []
      }
