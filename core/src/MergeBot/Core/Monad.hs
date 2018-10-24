{-|
Module      :  MergeBot.Core.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the monad used for the core functions of the merge bot.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.Monad
  ( BotAppT
  , runBot
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.ByteString.Char8 as ByteString
import Data.GraphQL
    ( MonadQuery(..)
    , QuerySettings(..)
    , QueryT
    , defaultQuerySettings
    , runQueryT
    )
import Network.HTTP.Client (requestHeaders)
import Network.HTTP.Types (hAuthorization, hUserAgent)

import MergeBot.Core.Config (BotConfig(..))

newtype BotAppT m a = BotAppT { unBotApp :: ReaderT BotConfig (QueryT m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader BotConfig
    )

instance MonadTrans BotAppT where
  lift = BotAppT . lift . lift

instance MonadIO m => MonadQuery (BotAppT m) where
  runQuerySafe query = BotAppT . lift . runQuerySafe query

runBot :: MonadIO m => BotConfig -> BotAppT m a -> m a
runBot config = runQueryT querySettings . (`runReaderT` config) . unBotApp
  where
    querySettings = defaultQuerySettings
      { url = "https://api.github.com/graphql"
      , modifyReq = \req -> req
          { requestHeaders =
              (hAuthorization, ByteString.pack $ "bearer " ++ githubToken config)
              : (hUserAgent, "LeapYear/merge-bot")
              : requestHeaders req
          }
      }
