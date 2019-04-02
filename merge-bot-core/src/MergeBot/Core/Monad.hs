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

import Control.Exception (displayException)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, handle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logErrorN)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
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
    ( GHEndpoint(..)
    , GitHubState(..)
    , GitHubT
    , KeyValue(..)
    , MonadGitHubREST(..)
    , runGitHubT
    )
import GitHub.REST.Auth (Token, fromToken)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Types (StdMethod(..), hAccept, hAuthorization, hUserAgent)

import MergeBot.Core.Error (getRelevantPRs)
import MergeBot.Core.GraphQL.API (API)
import MergeBot.Core.Logging (runMergeBotLogging)

-- | The monadic state in BotAppT.
data BotState = BotState
  { repoOwner :: Text
  , repoName  :: Text
  , appId     :: Int
  } deriving (Show)

newtype BotAppT m a = BotAppT
  { unBotApp ::
      LoggingT
        ( ReaderT BotState
          ( GitHubT
            ( QueryT API
                m
            )
          )
        )
        a
  }
  deriving (Functor,Applicative,Monad,MonadCatch,MonadIO,MonadLogger,MonadMask,MonadThrow)

instance MonadIO m => MonadGitHubREST (BotAppT m) where
  queryGitHub = BotAppT . lift . lift . queryGitHub

instance MonadIO m => MonadQuery API (BotAppT m) where
  runQuerySafe query = BotAppT . lift . lift . lift . runQuerySafe query

class (MonadMask m, MonadGitHubREST m, MonadQuery API m) => MonadMergeBot m where
  getRepo :: m (Text, Text)
  getAppId :: m Int

-- | 'asks' specialized to 'BotAppT'.
botAsks :: Monad m => (BotState -> a) -> BotAppT m a
botAsks = BotAppT . lift . asks

instance (MonadMask m, MonadIO m) => MonadMergeBot (BotAppT m) where
  getRepo = (,) <$> botAsks repoOwner <*> botAsks repoName
  getAppId = botAsks appId

data BotSettings = BotSettings
  { token     :: Token
  , repoOwner :: Text
  , repoName  :: Text
  , userAgent :: ByteString
  , appId     :: Int
  } deriving (Show)

runBotAppT :: (MonadMask m, MonadIO m) => BotSettings -> BotAppT m a -> m a
runBotAppT BotSettings{..} =
  runQueryT graphqlSettings
  . runGitHubT state
  . (`runReaderT` botState)
  . runMergeBotLogging
  . unBotApp
  . handle handleBotErr
  where
    state = GitHubState { token, userAgent, apiVersion = "antiope-preview" }
    botState = BotState{..}
    graphqlSettings = githubQuerySettings
      { modifyReq = \req -> req
        { requestHeaders =
            (hAuthorization, fromToken token)
            : (hUserAgent, userAgent)
            : (hAccept, "application/vnd.github.antiope-preview+json")
            : requestHeaders req
        }
      }
    handleBotErr e = do
      let msg = displayException e
      mapM_ (`commentOnPR` msg) $ getRelevantPRs e
      logErrorN $ Text.pack msg
      fail $ "[MergeBot Error] " ++ msg

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

-- | Add a comment to the given PR.
--
-- https://developer.github.com/v3/issues/comments/#create-a-comment
commentOnPR :: MonadMergeBot m => Int -> String -> m ()
commentOnPR prNum comment = void $ queryGitHub' GHEndpoint
  { method = POST
  , endpoint = "/repos/:owner/:repo/issues/:number/comments"
  , endpointVals = [ "number" := prNum ]
  , ghData = [ "body" := comment ]
  }
