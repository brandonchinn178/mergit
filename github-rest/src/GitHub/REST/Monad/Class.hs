{-|
Module      :  GitHub.REST.Monad.Class
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines 'MonadGitHubREST' that gives a monad @m@ the capability to query the GitHub REST API.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.REST.Monad.Class
  ( MonadGitHubREST(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value(..), eitherDecode, encode, object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Text as Text
import Network.HTTP.Client
    ( Manager
    , Request(..)
    , RequestBody(..)
    , Response(..)
    , httpLbs
    , parseRequest_
    , throwErrorStatusCodes
    )
import Network.HTTP.Types (hAccept, hAuthorization, hUserAgent)

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
