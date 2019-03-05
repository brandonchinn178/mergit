{-|
Module      :  Servant.GitHub.Combinators
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Servant combinators for serving a GitHub App.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.GitHub.Combinators
  ( GitHubSigned
  , GitHubEvent
  , GitHubAction
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Schema (IsSchemaObject, Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Network.Wai (Request, lazyRequestBody, requestHeaders)
import Servant
import qualified Servant.Server.Internal as Servant
import System.IO.Unsafe (unsafePerformIO)

import Servant.GitHub.Context (GitHubAppParams(..))
import Servant.GitHub.Event (GitHubEventType, IsGitHubEvent(..))
import Servant.GitHub.Security (doesSignatureMatch, parseSignature)

-- | A combinator for securing a path for only allowing valid signed requests sent from GitHub.
--
-- Usage:
--
-- > type MyApi = "webhook" :> GitHubSigned :> MyGitHubEvents
-- >
-- > -- These routes are guaranteed to only run if the request is signed by GitHub
-- > type MyGitHubEvents = ...
data GitHubSigned

instance
  ( HasServer api context
  , HasContextEntry context GitHubAppParams
  ) => HasServer (GitHubSigned :> api) context where

  type ServerT (GitHubSigned :> api) m = ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f s

  route _ context = route (Proxy @api) context . addAuthCheck_ verify
    where
      GitHubAppParams{ghWebhookSecret} = getContextEntry context

      verify request = do
        signature <- getSignature request
        ghSignature <- maybeDelayed invalidSignature $ parseSignature signature
        body <- liftIO $ getRequestBody False request signature

        unless (doesSignatureMatch ghWebhookSecret (ByteStringL.toStrict body) ghSignature)
          $ Servant.delayedFailFatal badSignature

      invalidSignature = err401 { errBody = "Invalid signature found" }
      badSignature = err401 { errBody = "Signature did not match payload" }

-- | A combinator for matching incoming requests with a given GitHub event.
--
-- Usage:
--
-- > import Servant.GitHub.Event (EventType(..))
-- > type MyGitHubEvents = GitHubEvent 'PushEvent :> GitHubAction
data GitHubEvent (event :: GitHubEventType)

instance
  ( HasServer api context
  , IsGitHubEvent event
  , IsSchemaObject (EventSchema event)
  ) => HasServer (GitHubEvent event :> api) context where

  type ServerT (GitHubEvent event :> api) m = Object (EventSchema event) -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  route _ context = route (Proxy @api) context . addBodyCheck parseBody . addHeaderCheck_ verify
    where
      verify request = do
        ghEvent <- maybeDelayed missingEvent $ lookup "x-github-event" $ requestHeaders request
        let e = ByteStringL.fromStrict ghEvent
        unless (e == event) $ Servant.delayedFail $ wrongEvent e

      parseBody request = do
        body <- getRequestBody' False request
        either fail return $ eitherDecode @(Object (EventSchema event)) body

      event = eventName @event
      missingEvent = err400 { errBody = "x-github-event header not found" }
      wrongEvent e = err400 { errBody = "Found event: " <> e <> " (expected: " <> event <> ")" }

-- TODO: 'WithToken' combinator

-- | An alias for @Post '[JSON] ()@, since GitHub sends JSON data, and webhook handlers shouldn't
-- return anything (they're only consumers).
--
-- This combinator MUST be used in order to clear the request body cache.
data GitHubAction

instance HasServer GitHubAction context where
  type ServerT GitHubAction m = m ()

  hoistServerWithContext _ _ nt s = nt s

  route _ context = route (Proxy @(Post '[JSON] ())) context . addPostBodyCheck_ clearCache
    where
      clearCache = void . getRequestBody' True

{- Request body caching

Have to do fun hacks since:
  1. 'lazyRequestBody' consumes the request body (so it can only be called once)
  2. Servant doesn't cache the request body

So we get to manually implement caching the request body YAY.
Ref: https://www.stackage.org/haddock/lts-13.10/servant-server-0.15/Servant-Server-Internal-RoutingApplication.html#t:Delayed
-}

requestBodyMapVar :: MVar (Map ByteString ByteStringL.ByteString)
{-# NOINLINE requestBodyMapVar #-}
requestBodyMapVar = unsafePerformIO $ newMVar Map.empty

getRequestBody :: Bool -> Request -> ByteString -> IO ByteStringL.ByteString
getRequestBody shouldDelete request signature = modifyMVar requestBodyMapVar $ \requestBodyMap ->
  case Map.lookup signature requestBodyMap of
    Nothing -> do
      body <- lazyRequestBody request
      return (Map.insert signature body requestBodyMap, body)
    Just body ->
      let requestBodyMap' = if shouldDelete
            then Map.delete signature requestBodyMap
            else requestBodyMap
      in return (requestBodyMap', body)

getRequestBody' :: Bool -> Request -> Servant.DelayedIO ByteStringL.ByteString
getRequestBody' shouldDelete request = liftIO . getRequestBody shouldDelete request =<< getSignature request

{- Helpers -}

maybeDelayed :: ServantErr -> Maybe a -> Servant.DelayedIO a
maybeDelayed e = maybe (Servant.delayedFailFatal e) return

getSignature :: Request -> Servant.DelayedIO ByteString
getSignature = maybeDelayed missingSignature . lookup "x-hub-signature" . requestHeaders
  where
    missingSignature = err400 { errBody = "x-hub-signature header not found" }

{- Servant internal functions -}

-- | The function I wish 'Servant.addAuthCheck' actually was.
addAuthCheck_ ::(Request -> Servant.DelayedIO ()) -> Servant.Delayed env a -> Servant.Delayed env a
addAuthCheck_ authCheck Servant.Delayed{..} =
  Servant.Delayed { Servant.authD = Servant.withRequest authCheck >> authD, .. }

-- | The function I wish 'Servant.addHeaderCheck' actually was.
addHeaderCheck_ :: (Request -> Servant.DelayedIO ()) -> Servant.Delayed env a -> Servant.Delayed env a
addHeaderCheck_ headerCheck Servant.Delayed{..} =
  Servant.Delayed { Servant.headersD = Servant.withRequest headerCheck >> headersD, .. }

-- | The function I wish 'Servant.addBodyCheck' actually was.
addBodyCheck :: (Request -> Servant.DelayedIO a) -> Servant.Delayed env (a -> b) -> Servant.Delayed env b
addBodyCheck bodyCheck Servant.Delayed{..} =
  Servant.Delayed
    { Servant.bodyD = \content -> do
        res <- Servant.withRequest bodyCheck
        b <- bodyD content
        return (res, b)
    , Servant.serverD = \c p h a (res, b) req -> ($ res) <$> serverD c p h a b req
    , ..
    }

-- | The function I wish 'Servant.addBodyCheck' also was.
addPostBodyCheck_ :: (Request -> Servant.DelayedIO ()) -> Servant.Delayed env a -> Servant.Delayed env a
addPostBodyCheck_ bodyCheck Servant.Delayed{..} =
  Servant.Delayed
    { Servant.bodyD = \content -> do
        res <- bodyD content
        Servant.withRequest bodyCheck
        return res
    , ..
    }
