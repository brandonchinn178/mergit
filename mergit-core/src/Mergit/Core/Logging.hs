{-|
Module      :  Mergit.Core.Logging
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for logging in Mergit.
-}
module Mergit.Core.Logging (
  runMergitLogging,
) where

import Control.Monad.Logger (
  LoggingT,
  defaultLogStr,
  fromLogStr,
  runLoggingT,
  toLogStr,
 )
import Data.ByteString.Char8 qualified as Char8
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

-- | Run the given action, sending logs to stdout.
runMergitLogging :: LoggingT m a -> m a
runMergitLogging = flip runLoggingT $ \loc src lvl str -> do
  now <- getCurrentTime
  let message = toLogStr (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> str
  Char8.putStr $ fromLogStr $ defaultLogStr loc src lvl message
