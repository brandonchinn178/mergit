{-|
Module      :  MergeBot.Server.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monads used in the MergeBot API.
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Server.Monad
  ( MergeBotEnv(..)
  , initEnv
  , MergeBotServer
  , MergeBotHandler
  , runMergeBotHandler
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Servant
import System.Environment (getEnv)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.Monad (BotAppT, runBot)
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
    , MonadIO
    , MonadReader MergeBotEnv
    )

instance MonadError ServantErr MergeBotHandler where
  throwError = MergeBotHandler . lift . lift . throwError
  catchError m f = do
    env <- ask
    MergeBotHandler . lift . lift $
      catchError (runMergeBotHandler env m) (runMergeBotHandler env . f)

-- | Run a MergeBotHandler with the given environment.
runMergeBotHandler :: MergeBotEnv -> MergeBotHandler a -> Handler a
runMergeBotHandler env = runBot (botConfig env) . (`runReaderT` env) . getHandler
