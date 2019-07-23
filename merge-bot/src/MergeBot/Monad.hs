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
  ( -- * Webhook monad
    BotApp
  , runBotApp
  , runBotApp'
  , runBotAppForAllInstalls
    -- * Debug monad
  , ServerDebug
  , DebugApp
  , runDebugApp
  , runBotAppDebug
  , getUser
  , withUser
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, lift, local, runReaderT)
import Data.Aeson.Schema (Object, get, schema)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.GraphQL (QuerySettings(..), QueryT, defaultQuerySettings, runQueryT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
    ( GHEndpoint(..)
    , GitHubState(..)
    , GitHubT
    , MonadGitHubREST(..)
    , Token
    , runGitHubT
    )
import GitHub.REST.Auth (fromToken, getJWTToken)
import GitHub.Schema.Repository (RepoWebhook)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types (StdMethod(..), hAccept, hAuthorization, hUserAgent)
import Servant (Handler, ServantErr(..), ServerT, err500)
import Servant.GitHub (GitHubAppParams(..), loadGitHubAppParams)
import Servant.GitHub.Security (getToken)
import UnliftIO (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import UnliftIO.Exception
    (SomeException, catch, displayException, fromException, throwIO, try)

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Monad (BotAppT, BotSettings(..), runBotAppT)

{- Webhook monad -}

type BotApp = BotAppT IO

-- | A helper around 'runBotAppT' for easy use by the Servant handlers.
runBotApp' :: Object RepoWebhook -> BotApp a -> Token -> Handler a
runBotApp' o action token = runIO $ runBotApp [get| o.full_name |] action token

-- | A helper around 'runBotAppT'.
runBotApp :: Text -> BotApp a -> Token -> IO a
runBotApp repo action token = do
  GitHubAppParams{ghUserAgent, ghAppId} <- liftIO loadGitHubAppParams
  let settings = BotSettings
        { userAgent = ghUserAgent
        , appId = ghAppId
        , ..
        }
  runBotAppT settings action
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

{- Debug monad -}

type ServerDebug api = ServerT api DebugApp

data DebugState = DebugState
  { debugToken :: Token
  , debugUser  :: Maybe Text
  }

newtype DebugApp a = DebugApp
  { unDebugApp ::
      ReaderT DebugState
        ( GitHubT
          ( QueryT API
              IO
          )
        )
        a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadGitHubREST DebugApp where
  queryGitHub = DebugApp . lift . queryGitHub

instance MonadError ServantErr DebugApp where
  throwError = throwIO
  catchError = catch

instance MonadUnliftIO DebugApp where
  askUnliftIO = DebugApp $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unDebugApp)

runDebugApp :: Token -> DebugApp a -> Handler a
runDebugApp token action = do
  GitHubAppParams{ghUserAgent} <- liftIO loadGitHubAppParams

  let ghState = GitHubState { token, userAgent = ghUserAgent, apiVersion = "antiope-preview" }
      graphqlSettings :: QuerySettings API
      graphqlSettings = defaultQuerySettings
        { url = "https://api.github.com/graphql"
        , modifyReq = \req -> req
          { requestHeaders =
              (hAuthorization, fromToken token)
              : (hUserAgent, ghUserAgent)
              : (hAccept, "application/vnd.github.antiope-preview+json")
              : requestHeaders req
          }
        }
      debugState = DebugState
        { debugToken = token
        , debugUser = Nothing
        }

  runIO
    . runQueryT graphqlSettings
    . runGitHubT ghState
    . (`runReaderT` debugState)
    . unDebugApp
    $ action

runBotAppDebug :: Text -> BotApp a -> DebugApp a
runBotAppDebug repo action = do
  token <- DebugApp $ asks debugToken
  liftIO $ runBotApp repo action token

-- | Get the currently authenticated user.
getUser :: DebugApp Text
getUser = DebugApp $ fromMaybe "Anonymous" <$> asks debugUser

withUser :: Text -> DebugApp a -> DebugApp a
withUser user = DebugApp . local (\state -> state { debugUser = Just user }) . unDebugApp

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
