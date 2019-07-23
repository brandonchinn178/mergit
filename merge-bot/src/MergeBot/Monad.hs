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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Monad
  ( -- * Webhook monad
    BotApp
  , runBotApp
  , runBotApp'
  , runBotAppForAllInstalls
    -- * Debug monad
  , DebugApp
  , runDebugApp
  , runBotAppDebug
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson.Schema (Object, get, schema)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.GraphQL (QuerySettings(..), QueryT, defaultQuerySettings, runQueryT)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
    (GHEndpoint(..), GitHubState(..), GitHubT, Token, queryGitHub, runGitHubT)
import GitHub.REST.Auth (fromToken, getJWTToken)
import GitHub.Schema.Repository (RepoWebhook)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types (StdMethod(..), hAccept, hAuthorization, hUserAgent)
import Servant (Handler, ServantErr(..), err500, throwError)
import Servant.GitHub (GitHubAppParams(..), loadGitHubAppParams)
import Servant.GitHub.Security (getToken)
import UnliftIO.Exception (SomeException, displayException, try)

import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Monad (BotAppT, BotSettings(..), runBotAppT)

{- Webhook monad -}

type BotApp = BotAppT IO

-- | A helper around 'runBotAppT' for easy use by the Servant handlers.
runBotApp :: Object RepoWebhook -> BotApp a -> Token -> Handler a
runBotApp o action token = runIO $ runBotApp' [get| o.full_name |] action token

-- | A helper around 'runBotAppT'.
runBotApp' :: Text -> BotApp a -> Token -> IO a
runBotApp' repo action token = do
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
      result <- runBotApp' repo action installToken
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

newtype DebugApp a = DebugApp
  { unDebugApp ::
      ReaderT Token
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

  runIO
    . runQueryT graphqlSettings
    . runGitHubT ghState
    . (`runReaderT` token)
    . unDebugApp
    $ action

runBotAppDebug :: Text -> BotApp a -> DebugApp a
runBotAppDebug repo action = do
  token <- DebugApp ask
  liftIO $ runBotApp' repo action token

{- Helpers -}

-- | Run the given IO action, throwing any exceptions as 500 errors.
runIO :: IO a -> Handler a
runIO m = liftIO (try @_ @SomeException m) >>= \case
  Right x -> return x
  Left e -> throwError $ err500 { errBody = Char8.pack $ displayException e }

-- | Separate a repo name of the format "owner/repo" into a tuple @(owner, repo)@.
parseRepo :: Text -> (Text, Text)
parseRepo repo = case Text.splitOn "/" repo of
  [repoOwner, repoName] -> (repoOwner, repoName)
  _ -> error $ "Invalid repo: " ++ Text.unpack repo
