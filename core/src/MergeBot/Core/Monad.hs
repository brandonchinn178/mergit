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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Monad
  ( BotAppT
  , runBot
  , BotEnv(..)
  , getRepo
  -- * Helpers
  , MonadREST
  , MonadGraphQL
  ) where

import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.GraphQL (MonadQuery(..), QueryT, runQueryT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GitHub (MonadGitHub, MonadREST, graphqlSettings)
import MergeBot.Core.GitHub.REST (KeyValue(..), MonadGitHub(..), githubAPI)
import MergeBot.Core.GraphQL.API (API)

type MonadGraphQL m = (MonadReader BotEnv m, MonadQuery API m)

data BotEnv = BotEnv
  { repoOwner :: String
  , repoName  :: String
  , ghToken   :: String
  , ghManager :: Manager
  }

-- | A helper to get the repoOwner and repoName as a pair.
getRepo :: BotEnv -> (String, String)
getRepo BotEnv{..} = (repoOwner, repoName)

newtype BotAppT m a = BotAppT { unBotApp :: ReaderT BotEnv (QueryT API m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadIO
    , MonadReader BotEnv
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift

instance MonadThrow m => MonadThrow (BotAppT m) where
  throwM = BotAppT . lift . lift . throwM

instance (MonadIO m, MonadCatch m) => MonadCatch (BotAppT m) where
  catch m f = do
    env <- ask
    BotAppT . lift . lift $
      catch (runBotWith env m) (runBotWith env . f)

instance MonadIO m => MonadQuery API (BotAppT m) where
  runQuerySafe query = BotAppT . lift . runQuerySafe query

instance MonadIO m => MonadGitHub (BotAppT m) where
  queryGitHub method endpoint vals ghData = do
    BotEnv{..} <- ask
    let vals' = vals ++ ["owner" :=* repoOwner, "repo" :=* repoName]
    githubAPI method endpoint vals' ghData ghToken ghManager

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
