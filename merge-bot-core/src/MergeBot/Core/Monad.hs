{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the monad used by the MergeBot.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Monad
  ( BotAppT
  , MonadMergeBot(..)
  , runBotAppT
  , queryGitHub'
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.GraphQL
    ( MonadQuery(..)
    , QuerySettings(..)
    , QueryT
    , defaultQuerySettings
    , runQueryT
    )
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.REST
    ( GHEndpoint(endpointVals)
    , GitHubState(..)
    , GitHubT
    , KeyValue(..)
    , MonadGitHubREST(..)
    , runGitHubT
    )
import GitHub.REST.Auth (Token, fromToken)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types (hAuthorization, hUserAgent)

import MergeBot.Core.GraphQL.API (API)

newtype BotAppT m a = BotAppT
  { unBotApp ::
      ReaderT (Text, Text)
        ( GitHubT
          ( QueryT API
              m
          )
        )
        a
  }
  deriving (Functor,Applicative,Monad,MonadCatch,MonadIO,MonadMask,MonadThrow)

instance MonadIO m => MonadGitHubREST (BotAppT m) where
  queryGitHub = BotAppT . lift . queryGitHub

instance MonadIO m => MonadQuery API (BotAppT m) where
  runQuerySafe query = BotAppT . lift . lift . runQuerySafe query

class (MonadMask m, MonadGitHubREST m, MonadQuery API m) => MonadMergeBot m where
  getRepo :: m (Text, Text)

instance (MonadMask m, MonadIO m) => MonadMergeBot (BotAppT m) where
  getRepo = BotAppT ask

runBotAppT :: MonadIO m => Token -> Text -> BotAppT m a -> m a
runBotAppT token repo =
  runQueryT (graphqlSettings token)
  . runGitHubT state
  . (`runReaderT` repo')
  . unBotApp
  where
    state = GitHubState
      { token
      , userAgent = botUserAgent
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

{- Settings -}

graphqlSettings :: Token -> QuerySettings API
graphqlSettings token = defaultQuerySettings
  { url = "https://api.github.com/graphql"
  , modifyReq = \req -> req
      { requestHeaders =
          (hAuthorization, fromToken token)
          : (hUserAgent, botUserAgent)
          : requestHeaders req
      }
  }

botUserAgent :: ByteString
botUserAgent = "LeapYear/merge-bot"
