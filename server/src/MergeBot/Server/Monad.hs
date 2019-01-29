{-|
Module      :  MergeBot.Server.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monads used in the MergeBot API.
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Server.Monad
  ( MergeBotEnv(..)
  , initEnv
  , MergeBotServer
  , MergeBotHandler
  , getBotState
  , getBotState'
  , runMergeBotHandler
  ) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Data.GraphQL (MonadQuery(..))
import Servant
import System.Environment (getEnv)

import MergeBot.Core.Config (BotConfig(..))
import qualified MergeBot.Core.GraphQL.API as Core
import MergeBot.Core.Monad (BotAppT, BotEnv, runBot)
import MergeBot.Core.State (BotState, newBotState)

-- | The environment shared by all API endpoints.
data MergeBotEnv = MergeBotEnv
  { botState  :: MVar BotState
  , botConfig :: BotConfig
  }

initEnv :: IO MergeBotEnv
initEnv = do
  cfgRepoOwner <- getEnv "BOT_REPO_OWNER"
  cfgRepoName <- getEnv "BOT_REPO_NAME"
  cfgToken <- getEnv "GITHUB_TOKEN"
  botState <- newMVar newBotState
  let botConfig = BotConfig{..}
  return MergeBotEnv{..}

-- | The ServerT monad for MergeBotHandler.
type MergeBotServer api = ServerT api MergeBotHandler

-- | The handler for the merge bot API endpoints.
newtype MergeBotHandler a = MergeBotHandler
  { getHandler :: ReaderT MergeBotEnv (BotAppT Handler) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ServantErr
    , MonadIO
    )

instance MonadReader BotEnv MergeBotHandler where
  ask = MergeBotHandler . lift $ ask
  local f (MergeBotHandler m) =
    MergeBotHandler . lift . local f . runReaderT m =<< MergeBotHandler ask

instance MonadQuery Core.API MergeBotHandler where
  runQuerySafe query = MergeBotHandler . lift . runQuerySafe query

getBotState :: MergeBotHandler (MVar BotState)
getBotState = MergeBotHandler $ asks botState

getBotState' :: MergeBotHandler BotState
getBotState' = MergeBotHandler $ liftIO . readMVar =<< asks botState

-- | Run a MergeBotHandler with the given environment.
runMergeBotHandler :: MergeBotEnv -> MergeBotHandler a -> Handler a
runMergeBotHandler env = runBot (botConfig env) . (`runReaderT` env) . getHandler
