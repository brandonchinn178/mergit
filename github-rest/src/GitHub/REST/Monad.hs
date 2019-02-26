{-|
Module      :  GitHub.REST.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'GitHubT' and 'MonadGitHubREST', a monad transformer and type class that gives a monad @m@
the capability to query the GitHub REST API.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module GitHub.REST.Monad
  ( MonadGitHubREST(..)
  , GitHubT
  , runGitHubT
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.ByteString (ByteString)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import GitHub.REST.Auth (Token)
import GitHub.REST.Monad.Class

data GitHubTState = GitHubTState
  { token     :: Token
  , manager   :: Manager
  , userAgent :: ByteString
  }

-- | A simple monad that can run REST calls.
newtype GitHubT m a = GitHubT
  { unGitHubT :: ReaderT GitHubTState m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadThrow
    , MonadTrans
    )

instance MonadIO m => MonadGitHubREST (GitHubT m) where
  getToken = GitHubT $ asks token
  getManager = GitHubT $ asks manager
  getUserAgent = GitHubT $ asks userAgent

-- | Run the given 'GitHubT' action with the given token and user agent.
--
-- The token will be sent with each API request -- see 'Token'. The user agent is also required for
-- each API request -- see https://developer.github.com/v3/#user-agent-required.
runGitHubT :: MonadIO m => Token -> ByteString -> GitHubT m a -> m a
runGitHubT token userAgent action = do
  manager <- liftIO $ newManager tlsManagerSettings
  runReaderT (unGitHubT action) GitHubTState{..}
