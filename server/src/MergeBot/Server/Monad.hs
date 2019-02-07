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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MergeBot.Server.Monad
  ( MergeBotEnv(..)
  , initEnv
  , MergeBotServer
  , MergeBotHandler
  , getBotState
  , getBotState'
  , runMergeBotHandler
  , updateBotState_
  , updateBotState
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.GraphQL (MonadQuery(..))
import Servant
import System.Environment (getEnv)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GitHub (MonadGitHub(..))
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
    , MonadBase IO
    , MonadCatch
    , MonadError ServantErr
    , MonadIO
    , MonadMask
    , MonadThrow
    )

instance MonadBaseControl IO MergeBotHandler where
  type StM MergeBotHandler a = StM Handler a
  liftBaseWith f = MergeBotHandler $ liftBaseWith $ \runInBase -> f (runInBase . getHandler)
  restoreM = MergeBotHandler . restoreM

instance MonadReader BotEnv MergeBotHandler where
  ask = MergeBotHandler . lift $ ask
  local f (MergeBotHandler m) =
    MergeBotHandler . lift . local f . runReaderT m =<< MergeBotHandler ask

instance MonadQuery Core.API MergeBotHandler where
  runQuerySafe query = MergeBotHandler . lift . runQuerySafe query

instance MonadGitHub MergeBotHandler where
  queryGitHub method endpoint endpointVals = MergeBotHandler . lift . queryGitHub method endpoint endpointVals

getBotState :: MergeBotHandler BotState
getBotState = MergeBotHandler $ liftIO . readMVar =<< asks botState

getBotState' :: MergeBotHandler (MVar BotState)
getBotState' = MergeBotHandler $ asks botState

-- | Run a MergeBotHandler with the given environment.
runMergeBotHandler :: MergeBotEnv -> MergeBotHandler a -> Handler a
runMergeBotHandler env = runBot (botConfig env) . (`runReaderT` env) . getHandler

{- State helpers -}

updateBotState_ :: (BotState -> MergeBotHandler BotState) -> MergeBotHandler ()
updateBotState_ f = updateBotState (fmap (, ()) . f)

updateBotState :: (BotState -> MergeBotHandler (BotState, a)) -> MergeBotHandler a
updateBotState runWithState = do
  stateMVar <- getBotState'

  result <- liftBaseWith $ \runInBase ->
    modifyMVar stateMVar $ \state ->
      fmap (fromResult state) $ runInBase $ runWithState state

  restoreM result
  where
    fromResult state1 = \case
      Right (state2, a) -> (state2, Right a)
      Left err          -> (state1, Left err)
