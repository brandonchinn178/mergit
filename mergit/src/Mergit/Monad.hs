{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      :  Mergit.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for running GitHubT actions.
-}
module Mergit.Monad (
  BaseApp,
  BaseAppConfig (..),
  ServerBase,
  runBaseApp,
  getGitHubAppParams,
  getAuthParams,

  -- * BaseApp helpers
  getInstallations,
  getRepoAndTokens,

  -- * BotAppT helpers
  BotApp,
  runBotApp,
  runBotAppOnAllRepos,

  -- * Queueing helpers
  MergitEvent (..),
  getEventRepo,
  handleEvents,
  queueEvent,
) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON)
import Data.Aeson.Schema (Object, get, schema)
import GitHub.REST (
  GHEndpoint (..),
  GitHubSettings (..),
  MonadGitHubREST (..),
  Token,
  runGitHubT,
 )
import GitHub.REST.Auth (getJWTToken)
import GitHub.Schema.Repository (Repository)
import Network.HTTP.Types (StdMethod (..))
import Servant (ServerError, ServerT)
import Servant.GitHub (GitHubAppParams (..))
import Servant.GitHub.Security (getToken)
import UnliftIO (MonadUnliftIO (..), wrappedWithRunInIO)
import UnliftIO.Exception (catch, throwIO)

import Mergit.Auth (AuthParams)
import Mergit.Core.GitHub (BranchName, CommitSHA, PrNum, Repo)
import Mergit.Core.Monad (BotAppT, MergitSettings (..), getRepo, runBotAppT)
import Mergit.EventQueue (
  EventQueuesManager,
  handleEventsWith,
  queueEventWith,
 )

{- The base monad for all servant routes -}

type ServerBase api = ServerT api BaseApp

data BaseAppConfig = BaseAppConfig
  { ghAppParams :: GitHubAppParams
  , authParams :: AuthParams
  , eventQueuesManager :: EventQueuesManager MergitEventKey MergitEvent
  }

newtype BaseApp a = BaseApp
  { unBaseApp :: ReaderT BaseAppConfig IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadUnliftIO BaseApp where
  withRunInIO = wrappedWithRunInIO BaseApp unBaseApp

instance MonadError ServerError BaseApp where
  throwError = throwIO
  catchError = catch

runBaseApp :: BaseAppConfig -> BaseApp a -> IO a
runBaseApp cfg = (`runReaderT` cfg) . unBaseApp

getGitHubAppParams :: BaseApp GitHubAppParams
getGitHubAppParams = BaseApp $ asks ghAppParams

getAuthParams :: BaseApp AuthParams
getAuthParams = BaseApp $ asks authParams

getEventQueuesManager :: BaseApp (EventQueuesManager MergitEventKey MergitEvent)
getEventQueuesManager = BaseApp $ asks eventQueuesManager

{- BaseApp helpers -}

-- | A helper for generating a JWT token.
getJWTToken' :: BaseApp Token
getJWTToken' = do
  GitHubAppParams{ghAppId, ghSigner} <- getGitHubAppParams
  liftIO $ getJWTToken ghSigner ghAppId

-- | A helper for querying GitHub using the given token.
queryGitHub' :: FromJSON a => GHEndpoint -> Token -> BaseApp a
queryGitHub' endpoint token = do
  GitHubAppParams{ghUserAgent} <- getGitHubAppParams
  let ghSettings =
        GitHubSettings
          { token = Just token
          , userAgent = ghUserAgent
          , apiVersion = "machine-man-preview"
          }

  liftIO $ runGitHubT ghSettings $ queryGitHub endpoint

-- | A helper for getting all the installation IDs for this GitHub app.
getInstallations :: BaseApp [Int]
getInstallations = do
  jwtToken <- getJWTToken'
  map [get| .id |] <$> getInstallations' jwtToken
  where
    getInstallations' =
      queryGitHub' @[Object [schema| { id: Int } |]]
        GHEndpoint
          { method = GET
          , endpoint = "/app/installations"
          , endpointVals = []
          , ghData = []
          }

{- BotAppT helpers -}

type BotApp = BotAppT BaseApp

-- | A helper around 'runBotAppT'.
runBotApp :: Repo -> BotApp a -> Token -> BaseApp a
runBotApp (repoOwner, repoName) action token = do
  GitHubAppParams{ghUserAgent, ghAppId} <- getGitHubAppParams
  let settings =
        MergitSettings
          { userAgent = ghUserAgent
          , appId = ghAppId
          , ..
          }
  runBotAppT settings action

getRepoAndTokens :: BaseApp [(Repo, Token)]
getRepoAndTokens = do
  GitHubAppParams{ghUserAgent, ghAppId, ghSigner} <- getGitHubAppParams

  -- get all installations
  installations <- getInstallations

  fmap concat $
    forM installations $ \installationId -> do
      -- get a token that can be used for this installation (expires in an hour)
      installToken <- liftIO $ getToken ghSigner ghAppId ghUserAgent installationId

      -- get list of repositories
      repositories <- [get| .repositories[].(owner.login, name) |] <$> getRepositories installToken

      pure $ map (,installToken) repositories
  where
    getRepositories =
      queryGitHub' @(Object [schema| { repositories: List #Repository } |])
        GHEndpoint
          { method = GET
          , endpoint = "/installation/repositories"
          , endpointVals = []
          , ghData = []
          }

-- | A helper that runs the given action for every repository that Mergit is installed on.
--
--  Returns a list of the results, along with the repository that produced each result.
runBotAppOnAllRepos :: BotApp a -> BaseApp [(Repo, a)]
runBotAppOnAllRepos action = mapM runOnRepo =<< getRepoAndTokens
  where
    runOnRepo (repo, token) = do
      result <- runBotApp repo action token
      pure (repo, result)

{- Queues -}

data MergitEventKey
  = OnPR Repo PrNum
  | OnBranch Repo BranchName
  | OnRepo Repo
  deriving (Show, Eq, Ord)

getEventRepo :: MergitEventKey -> Repo
getEventRepo = \case
  OnPR repo _ -> repo
  OnBranch repo _ -> repo
  OnRepo repo -> repo

-- | A Mergit event to be resolved serially.
--
--  In order to ensure that Mergit events are resolved atomically, Mergit code shouldn't run
--  state-modifying events directly, but rather queue events to be run serially in a separate
--  thread.
data MergitEvent
  = PRCreated PrNum CommitSHA
  | CommitPushedToPR PrNum CommitSHA
  | StartTryJob PrNum CommitSHA BranchName
  | QueuePR PrNum CommitSHA
  | DequeuePR PrNum CommitSHA
  | ResetMerge PrNum CommitSHA
  | RefreshCheckRun BranchName CommitSHA
  | DeleteBranch BranchName
  | PollQueues
  deriving (Show, Eq)

makeEventKey :: Repo -> MergitEvent -> MergitEventKey
makeEventKey repo = \case
  PRCreated prNum _ -> OnPR repo prNum
  CommitPushedToPR prNum _ -> OnPR repo prNum
  StartTryJob prNum _ _ -> OnPR repo prNum
  QueuePR prNum _ -> OnPR repo prNum
  DequeuePR prNum _ -> OnPR repo prNum
  ResetMerge prNum _ -> OnPR repo prNum
  RefreshCheckRun branch _ -> OnBranch repo branch
  DeleteBranch branch -> OnBranch repo branch
  PollQueues -> OnRepo repo

-- | A helper around 'handleEventsWith'
handleEvents :: (MergitEventKey -> MergitEvent -> BaseApp ()) -> BaseApp ()
handleEvents f = do
  eventQueuesManager <- getEventQueuesManager
  handleEventsWith eventQueuesManager f

-- | A helper around 'queueEventWith'
queueEvent :: MergitEvent -> BotApp ()
queueEvent event = do
  eventQueuesManager <- lift getEventQueuesManager
  repo <- getRepo
  let eventKey = makeEventKey repo event
  liftIO $ queueEventWith eventQueuesManager eventKey event
