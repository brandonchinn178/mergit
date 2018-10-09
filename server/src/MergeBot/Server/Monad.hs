{-|
Module      :  MergeBot.Server.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monads used in the MergeBot API.
-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MergeBot.Server.Monad
  ( MergeBotEnv(..)
  , initEnv
  , MergeBotServer
  , MergeBotHandler
  , runMergeBotHandler
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Servant

import MergeBot.Core.State (BotState, newBotState)

-- | The environment shared by all API endpoints.
newtype MergeBotEnv = MergeBotEnv
  { botState :: MVar BotState
  }

initEnv :: IO MergeBotEnv
initEnv = MergeBotEnv <$> newMVar newBotState

-- | The ServerT monad for MergeBotHandler.
type MergeBotServer api = ServerT api MergeBotHandler

-- | The handler for the merge bot API endpoints.
newtype MergeBotHandler a = MergeBotHandler
  { getHandler :: ReaderT MergeBotEnv Handler a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ServantErr
    , MonadIO
    , MonadReader MergeBotEnv
    )

-- | Run a MergeBotHandler with the given environment.
runMergeBotHandler :: MergeBotEnv -> MergeBotHandler a -> Handler a
runMergeBotHandler env = (`runReaderT` env) . getHandler
