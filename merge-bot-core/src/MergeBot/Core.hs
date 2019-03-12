{-|
Module      :  MergeBot.Core
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines core MergeBot functionality.
-}

module MergeBot.Core
  ( startTryJob
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import GitHub.REST (GitHubT)

startTryJob :: MonadIO m => Int -> GitHubT m ()
startTryJob pr = liftIO $ putStrLn $ "Start try job: " ++ show pr
