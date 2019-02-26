{-|
Module      :  GitHub.REST
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying the GitHub REST API.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.REST
  ( MonadGitHubREST(..)
  -- * GitHub authentication
  , Token(..)
  -- * GitHub Endpoints
  , GHEndpoint(..)
  , GitHubData
  , EndpointVals
  -- * KeyValue pairs
  , KeyValue(..)
  -- * Helpers
  , githubTry
  , (.:)
  ) where

import Control.Monad.Catch (MonadCatch, handleJust)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
    (FromJSON, Value(..), decode, eitherDecode, encode, object, withObject)
import Data.Aeson.Types (parseEither, parseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
    ( HttpException(..)
    , HttpExceptionContent(..)
    , Manager
    , Request(..)
    , RequestBody(..)
    , httpLbs
    , parseRequest_
    , responseBody
    , responseStatus
    , throwErrorStatusCodes
    )
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent, status422)

import GitHub.REST.Auth
import GitHub.REST.Endpoint
import GitHub.REST.KeyValue

-- | A type class for monads that can query the GitHub REST API.
--
-- Example:
-- > -- create the "foo" branch
-- > queryGitHub GHEndpoint
-- >   { method = POST
-- >   , endpoint = "/repos/:owner/:repo/git/refs"
-- >   , endpointVals =
-- >     [ "owner" := "alice"
-- >     , "repo" := "my-project"
-- >     ]
-- >   , ghData =
-- >     [ "ref" := "refs/heads/foo"
-- >     , "sha" := "1234567890abcdef"
-- >     ]
-- >   }
-- > -- delete the "foo" branch
-- > queryGitHub GHEndpoint
-- >   { method = DELETE
-- >   , endpoint = "/repos/:owner/:repo/git/refs/:ref"
-- >   , endpointVals =
-- >     [ "owner" := "alice"
-- >     , "repo" := "my-project"
-- >     , "ref" := "heads/foo"
-- >     ]
-- >   , ghData = []
-- >   }
class MonadIO m => MonadGitHubREST m where
  {-# MINIMAL getToken, getManager, getUserAgent | queryGitHub #-}

  getToken :: m Token
  getToken = error "No token specified"

  getManager :: m Manager
  getManager = error "No manager specified"

  -- | https://developer.github.com/v3/#user-agent-required
  getUserAgent :: m ByteString
  getUserAgent = error "No user agent specified"

  queryGitHub :: GHEndpoint -> m Value
  queryGitHub ghEndpoint = do
    manager <- getManager
    token <- getToken
    userAgent <- getUserAgent

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

    if ByteStringL.null response
      then return $ object []
      else either fail return $ eitherDecode response
    where
      getResponse manager = fmap responseBody . flip httpLbs manager
      request = parseRequest_ $ Text.unpack $ "https://api.github.com" <> endpointPath ghEndpoint

{- HTTP exception handling -}

-- | Handle any exceptions thrown by the GitHub REST API.
--
-- Assuming that all client errors will be error 422, since we should always be sending valid JSON.
-- https://developer.github.com/v3/#client-errors
githubTry :: MonadCatch m => m a -> m (Either Value a)
githubTry = handleJust statusException (return . Left) . fmap Right
  where
    statusException (HttpExceptionRequest _ (StatusCodeException r body))
      | responseStatus r == status422 = decode $ ByteStringL.fromStrict body
    statusException _ = Nothing

{- Aeson helpers -}

-- | Get the given key from the Value, erroring if it doesn't exist.
(.:) :: FromJSON a => Value -> Text -> a
(.:) v key = either error id $ parseEither parseObject v
  where
    parseObject = withObject "parseObject" (`parseField` key)
