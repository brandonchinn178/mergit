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
import Control.Monad.Trans.Class (MonadTrans)
import qualified Data.ByteString.Char8 as ByteString
import Data.GraphQL
    (MonadQuery, QuerySettings(..), QueryT, defaultQuerySettings, runQueryT)
import Network.HTTP.Client (requestHeaders)
import Network.HTTP.Types (hAuthorization, hUserAgent)

newtype BotAppT m a = BotAppT { unBotApp :: QueryT m a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadQuery,MonadTrans)

runBot :: MonadIO m => String -> BotAppT m a -> m a
runBot token = runQueryT querySettings . unBotApp
  where
    querySettings = defaultQuerySettings
      { url = "https://api.github.com/graphql"
      , modifyReq = \req -> req
          { requestHeaders =
              (hAuthorization, ByteString.pack $ "bearer " ++ token)
              : (hUserAgent, "LeapYear/merge-bot")
              : requestHeaders req
          }
      }
