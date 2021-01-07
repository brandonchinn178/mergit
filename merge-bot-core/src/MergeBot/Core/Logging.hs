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
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.FilePath ((</>))
import System.IO (IOMode(..), stderr, withFile)

-- | The directory for logs.
logDir :: FilePath
logDir = "/var/log/merge-bot/"

-- | Get the file to log to.
logDest :: UTCTime -> FilePath
logDest date = formatTime defaultTimeLocale "merge-bot__%Y-%m-%d.log" date

-- | Run the given action, sending logs to 'logDest' and 'stderr'.
runMergeBotLogging :: LoggingT m a -> m a
runMergeBotLogging = flip runLoggingT $ \loc src lvl str -> do
  now <- getCurrentTime
  let dest = logDir </> logDest now
      message = toLogStr (formatTime defaultTimeLocale "[%H:%M:%S] " now) <> str
      doLog h = Char8.hPutStr h $ fromLogStr $ defaultLogStr loc src lvl message

  withFile dest AppendMode doLog
  doLog stderr
