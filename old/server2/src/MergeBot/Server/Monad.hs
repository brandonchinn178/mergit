{-|
Module      :  MergeBot.Server.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monads used by the merge bot server.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MergeBot.Server.Monad
  ( AppState(..)
  , Handler
  , runHandler
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Text (Text)

import MergeBot.Core.State (BotState)

data AppState = AppState
  { ghToken  :: Text
  , botState :: MVar BotState -- TODO: this should go away
  }

newtype Handler a = Handler
  { getHandler :: ReaderT AppState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

runHandler :: AppState -> Handler a -> IO a
runHandler appState = (`runReaderT` appState) . getHandler
