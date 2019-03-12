{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the monad used by the MergeBot.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module MergeBot.Core.Monad
  ( BotAppT
  , MonadMergeBot
  , runBotAppT
  , queryGitHub'
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
    ( GHEndpoint(endpointVals)
    , GitHubState(..)
    , GitHubT
    , KeyValue(..)
    , MonadGitHubREST(..)
    , Token
    , runGitHubT
    )

newtype BotAppT m a = BotAppT
  { unBotApp :: ReaderT (Text, Text) (GitHubT m) a
  }
  deriving (Functor,Applicative,Monad,MonadCatch,MonadIO,MonadMask,MonadThrow)

instance MonadIO m => MonadGitHubREST (BotAppT m) where
  queryGitHub = BotAppT . lift . queryGitHub

class (MonadMask m, MonadGitHubREST m) => MonadMergeBot m where
  getRepo :: m (Text, Text)

instance (MonadMask m, MonadIO m) => MonadMergeBot (BotAppT m) where
  getRepo = BotAppT ask

runBotAppT :: MonadIO m => Token -> Text -> BotAppT m a -> m a
runBotAppT token repo = runGitHubT state . (`runReaderT` repo') . unBotApp
  where
    state = GitHubState
      { token
      , userAgent = "LeapYear/merge-bot"
      , apiVersion = "antiope-preview"
      }
    repo' = case Text.splitOn "/" repo of
      [repoOwner, repoName] -> (repoOwner, repoName)
      _ -> error $ "Invalid repo: " ++ Text.unpack repo

queryGitHub' :: MonadMergeBot m => GHEndpoint -> m Value
queryGitHub' endpoint = do
  (repoOwner, repoName) <- getRepo
  queryGitHub endpoint
    { endpointVals = endpointVals endpoint ++
      [ "owner" := repoOwner
      , "repo" := repoName
      ]
    }
