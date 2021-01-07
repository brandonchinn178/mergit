{-|
Module      :  MergeBot.Core.Logging
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for logging in the merge bot.
-}

module MergeBot.Core.Logging
  ( runMergeBotLogging
  ) where

import Control.Monad.Logger
    (LoggingT, defaultLogStr, fromLogStr, runLoggingT, toLogStr)
import qualified Data.ByteString.Char8 as Char8
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

-- | Run the given action, sending logs to stdout.
runMergeBotLogging :: LoggingT m a -> m a
runMergeBotLogging = flip runLoggingT $ \loc src lvl str -> do
  now <- getCurrentTime
  let message = toLogStr (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> str
  Char8.putStr $ fromLogStr $ defaultLogStr loc src lvl message
