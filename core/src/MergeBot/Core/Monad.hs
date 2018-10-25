{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monad used for the core functions of the merge bot.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Core.Monad
  ( BotAppT
  , runBot
  , BotEnv(..)
  , getRepo
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.GraphQL (MonadQuery(..), QueryT, runQueryT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GitHub (graphqlSettings)
import MergeBot.Core.GitHub.REST (KeyValue(..), MonadGitHub(..), githubAPI)

data BotEnv = BotEnv
  { repoOwner :: String
  , repoName  :: String
  , ghToken   :: String
  , ghManager :: Manager
  }

-- | A helper to get the repoOwner and repoName as a pair.
getRepo :: BotEnv -> (String, String)
getRepo BotEnv{..} = (repoOwner, repoName)

newtype BotAppT m a = BotAppT { unBotApp :: ReaderT BotEnv (QueryT m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader BotEnv
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift

instance MonadIO m => MonadQuery (BotAppT m) where
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
  runQueryT (graphqlSettings cfgToken)
    . (`runReaderT` env)
    . unBotApp
    $ app
