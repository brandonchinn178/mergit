{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monad used for the core functions of the merge bot.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module MergeBot.Core.Monad
  ( BotAppT(..)
  , MonadBotApp(..)
  , runBot
  -- * Helpers
  , queryGitHub'
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.GraphQL
    ( MonadQuery(..)
    , QuerySettings(..)
    , QueryT
    , defaultQuerySettings
    , runQueryT
    )
import GitHub.REST
    (GHEndpoint(..), GitHubT, KeyValue(..), MonadGitHubREST(..), runGitHubT)
import GitHub.REST.Auth (Token(..), fromToken)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types (hAuthorization, hUserAgent)

import MergeBot.Core.Config (BotConfig(..))
import MergeBot.Core.GraphQL.API (API)


data BotEnv = BotEnv
  { repoOwner :: String
  , repoName  :: String
  }

newtype BotAppT m a = BotAppT
  { unBotApp :: ReaderT BotEnv (GitHubT (QueryT API m)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadThrow
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift . lift

instance MonadIO m => MonadQuery API (BotAppT m) where
  runQuerySafe query = BotAppT . lift . lift . runQuerySafe query

instance MonadIO m => MonadGitHubREST (BotAppT m) where
  queryGitHub = BotAppT . lift . queryGitHub

class (MonadCatch m, MonadGitHubREST m) => MonadBotApp m where
  getRepo :: m (String, String)

instance (MonadCatch m, MonadIO m) => MonadBotApp (BotAppT m) where
  getRepo = do
    BotEnv{repoOwner, repoName} <- BotAppT ask
    return (repoOwner, repoName)

runBot :: MonadIO m => BotConfig -> BotAppT m a -> m a
runBot BotConfig{..} =
  runQueryT (graphqlSettings token)
  . runGitHubT token userAgent
  . (`runReaderT` env)
  . unBotApp
  where
    env = BotEnv
      { repoOwner = cfgRepoOwner
      , repoName = cfgRepoName
      }
    token = AccessToken $ Char8.pack cfgToken

{- Helpers -}

queryGitHub' :: MonadBotApp m => GHEndpoint -> m Value
queryGitHub' endpoint = do
  (repoOwner, repoName) <- getRepo
  queryGitHub endpoint
    { endpointVals = endpointVals endpoint ++
        [ "owner" := repoOwner
        , "repo" := repoName
        ]
    }

-- | Settings to query GitHub's GraphQL endpoint
graphqlSettings :: Token -> QuerySettings API
graphqlSettings token = defaultQuerySettings
  { url = "https://api.github.com/graphql"
  , modifyReq = \req -> req
      { requestHeaders =
          (hAuthorization, fromToken token)
          : (hUserAgent, userAgent)
          : requestHeaders req
      }
  }

-- | The user agent to use for the merge bot.
userAgent :: ByteString
userAgent = "LeapYear/merge-bot"
