{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines the monad used by the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MergeBot.Core.Monad
  ( BotAppT
  , runBotAppT
  , MonadMergeBot(..)
  , BotSettings(..)
  , queryGitHub'
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (displayException)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..), withUnliftIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logErrorN)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.GraphQL
    ( GraphQLQueryT
    , GraphQLSettings(..)
    , MonadGraphQLQuery(..)
    , defaultGraphQLSettings
    , runGraphQLQueryT
    )
import qualified Data.HashMap.Strict as HashMap
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
import Network.HTTP.Client
    (HttpException(..), HttpExceptionContent(..), Request(..), Response(..))
import Network.HTTP.Types
    (StdMethod(..), hAccept, hAuthorization, hUserAgent, status401)
import UnliftIO.Exception (Handler(..), SomeException, catchJust, catches)

import MergeBot.Core.Error (getRelevantPRs)
import MergeBot.Core.Logging (runMergeBotLogging)

-- | The monadic state in BotAppT.
data BotState = BotState
  { repoOwner :: Text
  , repoName  :: Text
  , appId     :: Int
  } deriving (Show)

newtype BotAppT m a = BotAppT
  { unBotAppT ::
      LoggingT
        ( ReaderT BotState
          ( GitHubT
            ( GraphQLQueryT
                m
            )
          )
        )
        a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift . lift . lift

instance MonadUnliftIO m => MonadGitHubREST (BotAppT m) where
  queryGitHubPage' = retry . (BotAppT . lift . lift . queryGitHubPage')
    where
      retry m =
        let go i = catchJust (getBadCredentialsError i) m $ \_ -> do
              liftIO $ threadDelay $ round (2 ** i :: Double)
              go (i + 1)
        in go 0

      getBadCredentialsError i = \case
        HttpExceptionRequest _ (StatusCodeException r body)
          | i < 5 -- only catch errors up to 5 times
          , responseStatus r == status401
          , Just o <- Aeson.decode (ByteStringL.fromStrict body)
          , Just (Aeson.String "Bad credentials") <- HashMap.lookup ("message" :: Text) o
          -> Just ()
        _ -> Nothing

instance MonadIO m => MonadGraphQLQuery (BotAppT m) where
  runQuerySafe = BotAppT . lift . lift . lift . runQuerySafe

instance MonadUnliftIO m => MonadUnliftIO (BotAppT m) where
  askUnliftIO = BotAppT $
    withUnliftIO $ \u ->
      return $ UnliftIO (unliftIO u . unBotAppT)

data BotSettings = BotSettings
  { token     :: Token
  , repoOwner :: Text
  , repoName  :: Text
  , userAgent :: ByteString
  , appId     :: Int
  } deriving (Show)

runBotAppT :: (MonadIO m, MonadUnliftIO m) => BotSettings -> BotAppT m a -> m a
runBotAppT BotSettings{..} =
  runGraphQLQueryT graphqlSettings
  . runGitHubT state
  . (`runReaderT` botState)
  . runMergeBotLogging
  . unBotAppT
  . (`catches`
      [ Handler handleBotErr
      , Handler handleSomeException
      ]
    )
  where
    state = GitHubState { token = Just token, userAgent, apiVersion = "antiope-preview" }
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
      logError msg
      errorWithoutStackTrace $ "MergeBot Error: " ++ msg
    handleSomeException (e :: SomeException) = do
      let msg = displayException e
      logError msg
      errorWithoutStackTrace $ "Other Error: " ++ msg

    -- log error message, replacing newlines with spaces
    logError msg = logErrorN $ removeNewlines $ Text.pack msg
    removeNewlines = Text.unwords . filter (not . Text.null) . Text.lines

{- MonadMergeBot class -}

class (MonadGitHubREST m, MonadGraphQLQuery m, MonadUnliftIO m) => MonadMergeBot m where
  getRepo :: m (Text, Text)
  getAppId :: m Int

-- | 'asks' specialized to 'BotAppT'.
botAsks :: Monad m => (BotState -> a) -> BotAppT m a
botAsks = BotAppT . lift . asks

instance (MonadIO m, MonadUnliftIO m) => MonadMergeBot (BotAppT m) where
  getRepo = (,) <$> botAsks repoOwner <*> botAsks repoName
  getAppId = botAsks appId

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

githubQuerySettings :: GraphQLSettings
githubQuerySettings = defaultGraphQLSettings
  { url = "https://api.github.com/graphql"
  }

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
