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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.GitHub.Combinators
  ( GitHubSigned
  , GitHubEvent
  , WithToken
  , WithToken'
  , TokenType(..)
  , GitHubAction
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Schema (IsSchemaObject, Object, get)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat, Nat, natVal)
import GitHub.REST (Token)
import GitHub.Schema.BaseEvent (BaseEvent)
import Network.Wai (Request)
import Servant
import qualified Servant.Server.Internal as Servant

import Servant.GitHub.Context (GitHubAppParams(..))
import Servant.GitHub.Event (GitHubEventType, IsGitHubEvent(..))
import Servant.GitHub.Internal.Request
import Servant.GitHub.Security (doesSignatureMatch, getToken, parseSignature)

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
        ghSignature <- maybe (Servant.delayedFailFatal invalidSignature) return $ parseSignature signature
        body <- liftIO $ getRequestBody' request signature

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
        ghEvent <- getGitHubEvent request
        let e = ByteStringL.fromStrict ghEvent
        unless (e == event) $ Servant.delayedFail $ wrongEvent e

      parseBody = decodeRequestBody @(EventSchema event)

      event = eventName @event
      wrongEvent e = err400 { errBody = "Found event: " <> e <> " (expected: " <> event <> ")" }

-- | A combinator for providing an access token to an endpoint that provides access to the GitHub
-- API. The token provided expires after 10 minutes. For different options, see 'WithToken''.
--
-- Usage:
--
-- > import GitHub.REST (Token)
-- > type MyGitHubEvents = WithToken :> GitHubAction
-- > server :: Server MyGitHubEvents
-- > server = handle
--
-- > handle :: Token -> Handler ()
-- > handle token = ...
type WithToken = WithToken' ('SingleToken 10)

data TokenType
  = SingleToken Nat
    -- ^ Generates a single token to use, with the given expiration in minutes
  | TokenMaker
    -- ^ Provides a value @Int -> IO Token@ that generates a token with the given expiration. Useful
    -- for hooks that might take a long time, and you want finer-grained control over token
    -- generation

data WithToken' (tokenType :: TokenType)

-- Generates a token that expires in the given number of minutes
type TokenGenerator = Int -> IO Token

mkTokenGenerator :: HasContextEntry context GitHubAppParams
  => Context context -> Request -> Servant.DelayedIO TokenGenerator
mkTokenGenerator context request = do
  event <- decodeRequestBody @BaseEvent request
  return $ getToken ghSigner ghUserAgent ghAppId [get| event.installation!.id |]
  where
    GitHubAppParams{ghAppId, ghSigner, ghUserAgent} = getContextEntry context

instance
  ( HasServer api context
  , HasContextEntry context GitHubAppParams
  , KnownNat expiry
  ) => HasServer (WithToken' ('SingleToken expiry) :> api) context where

  type ServerT (WithToken' ('SingleToken expiry) :> api) m = Token -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  -- TODO: fix the fact that a token is generated for every branch, even if the event didn't match
  route _ context = route (Proxy @api) context . addBodyCheck provideToken
    where
      provideToken request = do
        generator <- mkTokenGenerator context request
        liftIO . generator $ fromIntegral $ natVal (Proxy @expiry)

instance
  ( HasServer api context
  , HasContextEntry context GitHubAppParams
  ) => HasServer (WithToken' 'TokenMaker :> api) context where

  type ServerT (WithToken' 'TokenMaker :> api) m = TokenGenerator -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  route _ context = route (Proxy @api) context . addBodyCheck provideToken
    where
      provideToken = mkTokenGenerator context

-- | An alias for @Post '[JSON] ()@, since GitHub sends JSON data, and webhook handlers shouldn't
-- return anything (they're only consumers).
--
-- This combinator MUST be used in order to clear the request body cache.
data GitHubAction

instance HasServer GitHubAction context where
  type ServerT GitHubAction m = m ()

  hoistServerWithContext _ _ nt s = nt s

  route _ context = route (Proxy @(Post '[JSON] ())) context . addPostBodyCheck_ clearRequestBody

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
