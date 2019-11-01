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
  , getGitHubAppParams
  , getAuthParams
    -- * BaseApp helpers
  , runBaseHandler
  , getJWTToken'
  , queryGitHub'
  , getInstallations
    -- * BotAppT helpers
  , BotApp
  , runBotApp
  , runBotAppForAllInstalls
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (FromJSON)
import Data.Aeson.Schema (Object, get, schema)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GitHub.REST
    (GHEndpoint(..), GitHubState(..), MonadGitHubREST(..), Token, runGitHubT)
import GitHub.REST.Auth (getJWTToken)
import GitHub.Schema.Repository (Repository)
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

getGitHubAppParams :: BaseApp GitHubAppParams
getGitHubAppParams = BaseApp $ asks fst

getAuthParams :: BaseApp AuthParams
getAuthParams = BaseApp $ asks snd

{- BaseApp helpers -}

-- | A helper for running BaseApp in Handler.
runBaseHandler :: GitHubAppParams -> AuthParams -> BaseApp a -> Handler a
runBaseHandler ghAppParams authParams = runIO . runBaseApp ghAppParams authParams

-- | A helper for generating a JWT token.
getJWTToken' :: BaseApp Token
getJWTToken' = do
  GitHubAppParams{ghAppId, ghSigner} <- getGitHubAppParams
  liftIO $ getJWTToken ghSigner ghAppId

-- | A helper for querying GitHub using the given token.
queryGitHub' :: FromJSON a => GHEndpoint -> Token -> BaseApp a
queryGitHub' endpoint token = do
  GitHubAppParams{ghUserAgent} <- getGitHubAppParams
  let ghState = GitHubState
        { token = Just token
        , userAgent = ghUserAgent
        , apiVersion = "machine-man-preview"
        }

  liftIO $ runGitHubT ghState $ queryGitHub endpoint

-- | A helper for getting all the installation IDs for this GitHub app.
getInstallations :: BaseApp [Int]
getInstallations = do
  jwtToken <- getJWTToken'
  map [get| .id |] <$> getInstallations' jwtToken
  where
    getInstallations' = queryGitHub' @[Object [schema| { id: Int } |]] GHEndpoint
      { method = GET
      , endpoint = "/app/installations"
      , endpointVals = []
      , ghData = []
      }

{- BotAppT helpers -}

type BotApp = BotAppT IO

-- | A helper around 'runBotAppT'.
runBotApp :: Text -> Text -> BotApp a -> Token -> BaseApp a
runBotApp repoOwner repoName action token = do
  GitHubAppParams{ghUserAgent, ghAppId} <- getGitHubAppParams
  let settings = BotSettings
        { userAgent = ghUserAgent
        , appId = ghAppId
        , ..
        }
  liftIO $ runBotAppT settings action

-- | A helper that runs the given action for every repository that the merge bot is installed on.
--
-- Returns a list of the results, along with the repository that produced each result.
runBotAppForAllInstalls :: BotApp a -> BaseApp [(Text, a)]
runBotAppForAllInstalls action = do
  GitHubAppParams{ghUserAgent, ghAppId, ghSigner} <- getGitHubAppParams

  -- get all installations
  installations <- getInstallations

  -- run the given action in each repo
  fmap concat $ forM installations $ \installationId -> do
    installToken <- liftIO $ getToken ghSigner ghAppId ghUserAgent installationId
    repositories <- [get| .repositories[] |] <$> getRepositories installToken
    forM repositories $ \repo -> do
      let (repoOwner, repoName) = [get| repo.(owner.login, name) |]
      result <- runBotApp repoOwner repoName action installToken
      return ([get| repo.full_name |], result)
  where
    getRepositories = queryGitHub' @(Object [schema| { repositories: List #Repository } |]) GHEndpoint
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
