{-|
Module      :  MergeBot.Core.Logging
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions for logging in the merge bot.
-}

module MergeBot.Core.Logging
  ( LoggerIO
  , runMergeBotLogging
  ) where

import Control.Monad (unless)
import Control.Monad.Logger (LogLevel(..), LoggingT, defaultLogStr, runLoggingT)
import qualified Data.ByteString.Char8 as Char8
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.FilePath ((</>))
import System.IO (IOMode(..), stderr, withFile)
import System.Log.FastLogger (fromLogStr)

-- | A type alias for a basic logger running in IO.
type LoggerIO = LoggingT IO

-- | The directory for logs.
logDir :: FilePath
logDir = "/var/log/merge-bot/"

-- | Get the file to log to.
logDest :: UTCTime -> FilePath
logDest date = "merge-bot__" <> date' <> ".log"
  where
    date' = formatTime defaultTimeLocale "%Y-%m-%d" date

-- | Run the given action, sending logs to 'getLogDest' and 'stderr'.
runMergeBotLogging :: LoggingT m a -> m a
runMergeBotLogging = flip runLoggingT $ \loc src lvl str -> do
  now <- getCurrentTime
  let dest = logDir </> logDest now
      doLog h = Char8.hPutStr h $ fromLogStr $ defaultLogStr loc src lvl str

  withFile dest AppendMode doLog

  -- error logs are already sent to stderr via 'fail' (see 'handleBotErr')
  unless (lvl == LevelError) $
    doLog stderr
