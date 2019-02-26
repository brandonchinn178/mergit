{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monad used for the core functions of the merge bot.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MergeBot.Core.Monad
  ( BotAppT(..)
  , MonadBotApp(..)
  , runBot
  -- * Helpers
  , MonadGraphQL
  , queryGitHub'
  ) where

import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control
    ( ComposeSt
    , MonadBaseControl(..)
    , MonadTransControl(..)
    , defaultLiftBaseWith
    , defaultLiftWith2
    , defaultRestoreM
    , defaultRestoreT2
    )
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as Char8
import Data.GraphQL
    ( MonadQuery(..)
    , QuerySettings(..)
    , QueryT
    , defaultQuerySettings
    , runQueryT
    )
import GitHub.REST
    (GHEndpoint(..), KeyValue(..), MonadGitHubREST(..), Token(..))
import Network.HTTP.Client (Manager, Request(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAuthorization, hUserAgent)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GraphQL.API (API)

type MonadGraphQL m = (MonadBotApp m, MonadQuery API m)

data BotEnv = BotEnv
  { repoOwner :: String
  , repoName  :: String
  , ghToken   :: String
  , ghManager :: Manager
  }

newtype BotAppT m a = BotAppT { unBotApp :: ReaderT BotEnv (QueryT API m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadError e
    , MonadIO
    , MonadMask
    , MonadReader BotEnv
    , MonadThrow
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift

instance MonadBase IO m => MonadBase IO (BotAppT m) where
  liftBase = liftBaseDefault

instance MonadTransControl BotAppT where
  type StT BotAppT a = StT (ReaderT BotEnv) (StT (QueryT API) a)
  liftWith = defaultLiftWith2 BotAppT unBotApp
  restoreT = defaultRestoreT2 BotAppT

instance MonadBaseControl IO m => MonadBaseControl IO (BotAppT m) where
  type StM (BotAppT m) a = ComposeSt BotAppT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadIO m => MonadQuery API (BotAppT m) where
  runQuerySafe query = BotAppT . lift . runQuerySafe query

instance MonadIO m => MonadGitHubREST (BotAppT m) where
  getToken = asks (AccessToken . Char8.pack . ghToken)
  getManager = asks ghManager
  getUserAgent = pure "LeapYear/merge-bot"

class (MonadCatch m, MonadGitHubREST m) => MonadBotApp m where
  getRepo :: m (String, String)

instance (MonadCatch m, MonadIO m) => MonadBotApp (BotAppT m) where
  getRepo = do
    BotEnv{repoOwner, repoName} <- BotAppT ask
    return (repoOwner, repoName)

runBot :: MonadIO m => BotConfig -> BotAppT m a -> m a
runBot BotConfig{..} app = do
  manager <- liftIO $ newManager tlsManagerSettings
  let env = BotEnv
        { repoOwner = cfgRepoOwner
        , repoName = cfgRepoName
        , ghToken = cfgToken
        , ghManager = manager
        }
  runBotWith env app

runBotWith :: MonadIO m => BotEnv -> BotAppT m a -> m a
runBotWith env = runQueryT (graphqlSettings $ ghToken env) . (`runReaderT` env) . unBotApp

{- Helpers -}

queryGitHub' :: MonadBotApp m => GHEndpoint -> m Value
queryGitHub' endpoint = do
  (repoOwner, repoName) <- getRepo
  queryGitHub endpoint
    { endpointVals = endpointVals endpoint ++
        [ "owner" := repoOwner
        , "repo" := repoName
        ]
    }

-- | Settings to query GitHub's GraphQL endpoint
graphqlSettings :: String -> QuerySettings API
graphqlSettings token = defaultQuerySettings
  { url = "https://api.github.com/graphql"
  , modifyReq = \req -> req
      { requestHeaders =
          (hAuthorization, Char8.pack $ "bearer " ++ token)
          : (hUserAgent, "LeapYear/merge-bot")
          : requestHeaders req
      }
  }
