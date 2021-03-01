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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Monad
  ( BaseApp
  , BaseAppConfig(..)
  , ServerBase
  , runBaseApp
  , getGitHubAppParams
  , getAuthParams
    -- * BaseApp helpers
  , getInstallations
  , getRepoAndTokens
    -- * BotAppT helpers
  , BotApp
  , runBotApp
  , runBotAppOnAllRepos
    -- * Queueing helpers
  , MergeBotEvent(..)
  , getEventRepo
  , handleEvents
  , queueEvent
  ) where

import Control.Monad (forM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON)
import Data.Aeson.Schema (Object, get, schema)
import GitHub.REST
    (GHEndpoint(..), GitHubState(..), MonadGitHubREST(..), Token, runGitHubT)
import GitHub.REST.Auth (getJWTToken)
import GitHub.Schema.Repository (Repository)
import Network.HTTP.Types (StdMethod(..))
import Servant (ServerError, ServerT)
import Servant.GitHub (GitHubAppParams(..))
import Servant.GitHub.Security (getToken)
import UnliftIO (MonadUnliftIO(..), wrappedWithRunInIO)
import UnliftIO.Exception (catch, throwIO)

import MergeBot.Auth (AuthParams)
import MergeBot.Core.GitHub (BranchName, CheckRunId, CommitSHA, PrNum, Repo)
import MergeBot.Core.Monad (BotAppT, BotSettings(..), getRepo, runBotAppT)
import MergeBot.EventQueue (MergeBotQueues, handleEventsWith, queueEventWith)

{- The base monad for all servant routes -}

type ServerBase api = ServerT api BaseApp

data BaseAppConfig = BaseAppConfig
  { ghAppParams    :: GitHubAppParams
  , authParams     :: AuthParams
  , mergeBotQueues :: MergeBotQueues EventKey MergeBotEvent
  }

newtype BaseApp a = BaseApp
  { unBaseApp :: ReaderT BaseAppConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

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

getMergeBotQueues :: BaseApp (MergeBotQueues EventKey MergeBotEvent)
getMergeBotQueues = BaseApp $ asks mergeBotQueues

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

type BotApp = BotAppT BaseApp

-- | A helper around 'runBotAppT'.
runBotApp :: Repo -> BotApp a -> Token -> BaseApp a
runBotApp (repoOwner, repoName) action token = do
  GitHubAppParams{ghUserAgent, ghAppId} <- getGitHubAppParams
  let settings = BotSettings
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

  fmap concat $ forM installations $ \installationId -> do
    -- get a token that can be used for this installation (expires in an hour)
    installToken <- liftIO $ getToken ghSigner ghAppId ghUserAgent installationId

    -- get list of repositories
    repositories <- [get| .repositories[].(owner.login, name) |] <$> getRepositories installToken

    return $ map (, installToken) repositories
  where
    getRepositories = queryGitHub' @(Object [schema| { repositories: List #Repository } |]) GHEndpoint
      { method = GET
      , endpoint = "/installation/repositories"
      , endpointVals = []
      , ghData = []
      }

-- | A helper that runs the given action for every repository that the merge bot is installed on.
--
-- Returns a list of the results, along with the repository that produced each result.
runBotAppOnAllRepos :: BotApp a -> BaseApp [(Repo, a)]
runBotAppOnAllRepos action = mapM runOnRepo =<< getRepoAndTokens
  where
    runOnRepo (repo, token) = do
      result <- runBotApp repo action token
      return (repo, result)

{- Queues -}

data EventKey
  = OnPR Repo PrNum
  | OnBranch Repo BranchName
  | OnRepo Repo
  deriving (Show, Eq, Ord)

getEventRepo :: EventKey -> Repo
getEventRepo = \case
  OnPR repo _ -> repo
  OnBranch repo _ -> repo
  OnRepo repo -> repo

-- | A merge bot event to be resolved serially.
--
-- In order to ensure that mergebot events are resolved atomically, merge-bot code shouldn't run
-- state-modifying events directly, but rather queue events to be run serially in a separate
-- thread.
data MergeBotEvent
  = PRCreated PrNum CommitSHA
  | CommitPushedToPR PrNum CommitSHA
  | StartTryJob PrNum CommitSHA BranchName CheckRunId
  | QueuePR PrNum CommitSHA
  | DequeuePR PrNum CommitSHA
  | ResetMerge PrNum CommitSHA
  | RefreshCheckRun BranchName CommitSHA
  | DeleteBranch BranchName
  | PollQueues
  deriving (Show, Eq)

makeEventKey :: Repo -> MergeBotEvent -> EventKey
makeEventKey repo = \case
  PRCreated prNum _        -> OnPR repo prNum
  CommitPushedToPR prNum _ -> OnPR repo prNum
  StartTryJob prNum _ _ _  -> OnPR repo prNum
  QueuePR prNum _          -> OnPR repo prNum
  DequeuePR prNum _        -> OnPR repo prNum
  ResetMerge prNum _       -> OnPR repo prNum
  RefreshCheckRun branch _ -> OnBranch repo branch
  DeleteBranch branch      -> OnBranch repo branch
  PollQueues               -> OnRepo repo

-- | A helper around 'handleEventsWith'
handleEvents :: (EventKey -> MergeBotEvent -> BaseApp ()) -> BaseApp ()
handleEvents f = do
  mergeBotQueues <- getMergeBotQueues
  handleEventsWith mergeBotQueues f

-- | A helper around 'queueEventWith'
queueEvent :: MergeBotEvent -> BotApp ()
queueEvent event = do
  mergeBotQueues <- lift getMergeBotQueues
  repo <- getRepo
  let eventKey = makeEventKey repo event
  liftIO $ queueEventWith mergeBotQueues eventKey event
