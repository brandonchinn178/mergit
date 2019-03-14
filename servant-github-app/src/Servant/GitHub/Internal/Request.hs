{-|
Module      :  Servant.GitHub.Internal.Request
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Helper functions for getting information from Request objects.
-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.GitHub.Internal.Request where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.CaseInsensitive (CI, original)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Wai (Request, lazyRequestBody, requestHeaders)
import Servant
import qualified Servant.Server.Internal as Servant
import System.IO.Unsafe (unsafePerformIO)

{-
Request Body Caching

We have to implement caching of request bodies, since:
  1. 'lazyRequestBody' consumes the request body (so it can only be called once)
  2. Servant doesn't cache the request body

So we get to manually implement caching the request body YAY.
Ref: https://www.stackage.org/haddock/lts-13.10/servant-server-0.15/Servant-Server-Internal-RoutingApplication.html#t:Delayed
-}

{-# NOINLINE requestBodyMapVar #-}
requestBodyMapVar :: MVar (Map ByteString ByteStringL.ByteString)
requestBodyMapVar = unsafePerformIO $ newMVar Map.empty

getRequestBody' :: Request -> ByteString -> IO ByteStringL.ByteString
getRequestBody' request signature = modifyMVar requestBodyMapVar $ \requestBodyMap ->
  case Map.lookup signature requestBodyMap of
    Nothing -> do
      body <- lazyRequestBody request
      return (Map.insert signature body requestBodyMap, body)
    Just body -> return (requestBodyMap, body)

getRequestBody :: Request -> Servant.DelayedIO ByteStringL.ByteString
getRequestBody request = liftIO . getRequestBody' request =<< getSignature request

clearRequestBody :: Request -> Servant.DelayedIO ()
clearRequestBody request = do
  signature <- getSignature request
  liftIO $ modifyMVar_ requestBodyMapVar $ pure . Map.delete signature

{- Headers -}

getHeader :: CI ByteString -> Request -> Servant.DelayedIO ByteString
getHeader header = maybe (Servant.delayedFailFatal e) return . lookup header . requestHeaders
  where
    e = err400 { errBody = ByteStringL.fromStrict (original header) <> " header not found" }

getSignature :: Request -> Servant.DelayedIO ByteString
getSignature = getHeader "x-hub-signature"

getGitHubEvent :: Request -> Servant.DelayedIO ByteString
getGitHubEvent = getHeader "x-github-event"
