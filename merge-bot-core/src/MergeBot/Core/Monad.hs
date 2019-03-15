{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the monad used by the MergeBot.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MergeBot.Core.Monad
  ( BotAppT
  , MonadMergeBot(..)
  , BotSettings(..)
  , runBotAppT
  , queryGitHub'
  -- * Helpers
  , parseRepo
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

-- | The monadic state in BotAppT.
data BotState = BotState
  { repoOwner :: Text
  , repoName  :: Text
  } deriving (Show)

newtype BotAppT m a = BotAppT
  { unBotApp ::
      ReaderT BotState
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
  getRepo = BotAppT $ do
    BotState{..} <- ask
    return (repoOwner, repoName)

data BotSettings = BotSettings
  { token     :: Token
  , repoOwner :: Text
  , repoName  :: Text
  , userAgent :: ByteString
  } deriving (Show)

runBotAppT :: MonadIO m => BotSettings -> BotAppT m a -> m a
runBotAppT BotSettings{..} =
  runQueryT graphqlSettings
  . runGitHubT state
  . (`runReaderT` botState)
  . unBotApp
  where
    state = GitHubState { token, userAgent, apiVersion = "antiope-preview" }
    botState = BotState{..}
    graphqlSettings = githubQuerySettings
      { modifyReq = \req -> req
        { requestHeaders =
            (hAuthorization, fromToken token)
            : (hUserAgent, userAgent)
            : requestHeaders req
        }
      }

queryGitHub' :: MonadMergeBot m => GHEndpoint -> m Value
queryGitHub' endpoint = do
  (repoOwner, repoName) <- getRepo
  queryGitHub endpoint
    { endpointVals = endpointVals endpoint ++
      [ "owner" := repoOwner
      , "repo" := repoName
      ]
    }

{- Helpers -}

githubQuerySettings :: QuerySettings API
githubQuerySettings = defaultQuerySettings
  { url = "https://api.github.com/graphql"
  }

-- | Separate a repo name of the format "owner/repo" into a tuple @(owner, repo)@.
parseRepo :: Text -> (Text, Text)
parseRepo repo = case Text.splitOn "/" repo of
  [repoOwner, repoName] -> (repoOwner, repoName)
  _ -> error $ "Invalid repo: " ++ Text.unpack repo
