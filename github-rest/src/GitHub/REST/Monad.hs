{-|
Module      :  GitHub.REST.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'GitHubT' and 'MonadGitHubREST', a monad transformer and type class that gives a monad @m@
the capability to query the GitHub REST API.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GitHub.REST.Monad
  ( MonadGitHubREST(..)
  , GitHubT
  , GitHubState(..)
  , runGitHubT
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.Aeson (eitherDecode, encode, object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Text as Text
import Network.HTTP.Client
    ( Manager
    , Request(..)
    , RequestBody(..)
    , Response(..)
    , httpLbs
    , newManager
    , parseRequest_
    , throwErrorStatusCodes
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)

import GitHub.REST.Auth (Token, fromToken)
import GitHub.REST.Endpoint (GHEndpoint(..), endpointPath, renderMethod)
import GitHub.REST.KeyValue (kvToValue)
import GitHub.REST.Monad.Class

data GitHubState = GitHubState
  { token     :: Token
  , userAgent :: ByteString
  }

-- | A simple monad that can run REST calls.
newtype GitHubT m a = GitHubT
  { unGitHubT :: ReaderT (Manager, GitHubState) m a
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
  queryGitHub ghEndpoint = do
    (manager, GitHubState{..}) <- GitHubT ask
    response <- liftIO $ getResponse manager request
      { method = renderMethod ghEndpoint
      , requestHeaders =
          (hAccept, "application/vnd.github.machine-man-preview+json")
          : (hUserAgent, userAgent)
          : (hAuthorization, fromToken token)
          : requestHeaders request
      , requestBody = RequestBodyLBS $ encode $ kvToValue $ ghData ghEndpoint
      , checkResponse = throwErrorStatusCodes
      }

    either fail return . eitherDecode $
      if ByteStringL.null response
        then encode $ object []
        else response
    where
      getResponse manager = fmap responseBody . flip httpLbs manager
      request = parseRequest_ $ Text.unpack $ "https://api.github.com" <> endpointPath ghEndpoint

-- | Run the given 'GitHubT' action with the given token and user agent.
--
-- The token will be sent with each API request -- see 'Token'. The user agent is also required for
-- each API request -- see https://developer.github.com/v3/#user-agent-required.
runGitHubT :: MonadIO m => GitHubState -> GitHubT m a -> m a
runGitHubT state action = do
  manager <- liftIO $ newManager tlsManagerSettings
  (`runReaderT` (manager, state)) . unGitHubT $ action
