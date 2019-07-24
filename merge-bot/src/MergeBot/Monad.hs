{-|
Module      :  MergeBot.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for running GitHubT actions.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Monad
  ( BaseApp
  , ServerBase
  , runBaseApp
  , runBaseHandler
  , getGitHubAppParams
  , getAuthParams
    -- * BotAppT helpers
  , BotApp
  , runBotApp
  , runBotAppForAllInstalls
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson.Schema (Object, get, schema)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
    (GHEndpoint(..), GitHubState(..), MonadGitHubREST(..), Token, runGitHubT)
import GitHub.REST.Auth (getJWTToken)
import Network.HTTP.Types (StdMethod(..))
import Servant (Handler, ServantErr(..), ServerT, err500)
import Servant.GitHub (GitHubAppParams(..))
import Servant.GitHub.Security (getToken)
import UnliftIO (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import UnliftIO.Exception
    (SomeException, catch, displayException, fromException, throwIO, try)

import MergeBot.Auth (AuthParams)
import MergeBot.Core.Monad (BotAppT, BotSettings(..), runBotAppT)

{- The base monad for all servant routes -}

type ServerBase api = ServerT api BaseApp

newtype BaseApp a = BaseApp
  { unBaseApp :: ReaderT (GitHubAppParams, AuthParams) IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnliftIO BaseApp where
  askUnliftIO = BaseApp $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unBaseApp)

instance MonadError ServantErr BaseApp where
  throwError = throwIO
  catchError = catch

runBaseApp :: GitHubAppParams -> AuthParams -> BaseApp a -> IO a
runBaseApp ghAppParams authParams = (`runReaderT` state) . unBaseApp
  where
    state = (ghAppParams, authParams)

-- | A helper for running BaseApp in Handler.
runBaseHandler :: GitHubAppParams -> AuthParams -> BaseApp a -> Handler a
runBaseHandler ghAppParams authParams = runIO . runBaseApp ghAppParams authParams

getGitHubAppParams :: BaseApp GitHubAppParams
getGitHubAppParams = BaseApp $ asks fst

getAuthParams :: BaseApp AuthParams
getAuthParams = BaseApp $ asks snd

{- BotAppT helpers -}

type BotApp = BotAppT IO

-- | A helper around 'runBotAppT'.
runBotApp :: Text -> BotApp a -> Token -> BaseApp a
runBotApp repo action token = do
  GitHubAppParams{ghUserAgent, ghAppId} <- getGitHubAppParams
  let settings = BotSettings
        { userAgent = ghUserAgent
        , appId = ghAppId
        , ..
        }
  liftIO $ runBotAppT settings action
  where
    (repoOwner, repoName) = parseRepo repo

-- | A helper that runs the given action for every repository that the merge bot is installed on.
--
-- Returns a list of the results, along with the repository that produced each result.
runBotAppForAllInstalls :: BotApp a -> BaseApp [(Text, a)]
runBotAppForAllInstalls action = do
  GitHubAppParams{ghUserAgent, ghAppId, ghSigner} <- getGitHubAppParams

  -- get all installations
  jwtToken <- liftIO $ getJWTToken ghSigner ghAppId
  let mkState token = GitHubState
        { token
        , userAgent = ghUserAgent
        , apiVersion = "machine-man-preview"
        }
      jwtState = mkState jwtToken
  installations <- liftIO $ map [get| .id |] <$> runGitHubT jwtState getInstallations

  -- run the given action in each repo
  fmap concat $ forM installations $ \installationId -> do
    installToken <- liftIO $ getToken ghSigner ghAppId ghUserAgent installationId
    let state = mkState installToken
    repositories <- liftIO $ [get| .repositories[].full_name |] <$> runGitHubT state getRepositories
    forM repositories $ \repo -> do
      result <- runBotApp repo action installToken
      return (repo, result)
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

{- Helpers -}

-- | Run the given IO action, throwing any exceptions as 500 errors.
runIO :: IO a -> Handler a
runIO m = liftIO (try @_ @SomeException m) >>= \case
  Right x -> return x
  Left e -> throwError . fromMaybe (showErr e) . fromException $ e
  where
    showErr e = err500 { errBody = Char8.pack $ displayException e }

-- | Separate a repo name of the format "owner/repo" into a tuple @(owner, repo)@.
parseRepo :: Text -> (Text, Text)
parseRepo repo = case Text.splitOn "/" repo of
  [repoOwner, repoName] -> (repoOwner, repoName)
  _ -> error $ "Invalid repo: " ++ Text.unpack repo
