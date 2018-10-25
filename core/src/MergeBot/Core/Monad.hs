{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monad used for the core functions of the merge bot.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MergeBot.Core.Monad
  ( BotAppT
  , runBot
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.GraphQL (MonadQuery(..), QueryT, runQueryT)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GitHub (graphqlSettings)
import MergeBot.Core.GitHub.REST (MonadGitHub(..), githubAPI)

newtype BotAppT m a = BotAppT { unBotApp :: ReaderT BotConfig (QueryT m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader BotConfig
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift

instance MonadIO m => MonadQuery (BotAppT m) where
  runQuerySafe query = BotAppT . lift . runQuerySafe query

instance MonadIO m => MonadGitHub (BotAppT m) where
  queryGitHub method endpoint vals ghData = do
    token <- asks githubToken
    manager <- liftIO $ newManager tlsManagerSettings
    githubAPI method endpoint vals ghData token manager

runBot :: MonadIO m => BotConfig -> BotAppT m a -> m a
runBot config =
  runQueryT (graphqlSettings $ githubToken config)
    . (`runReaderT` config)
    . unBotApp
