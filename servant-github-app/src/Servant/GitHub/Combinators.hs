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
  ( GitHubEvent
  , WithToken
  , WithToken'
  , TokenType(..)
  , GitHubAction
  ) where

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Schema (IsSchemaObject, Object, SchemaType, get)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Proxy (Proxy(..))
import GitHub.REST (Token)
import GitHub.Schema.BaseEvent (BaseEvent)
import Network.Wai (Request)
import Servant
import qualified Servant.Server.Internal as Servant

import Servant.GitHub.Context (GitHubAppParams(..))
import Servant.GitHub.Event (GitHubEventType, IsGitHubEvent(..))
import Servant.GitHub.Internal.Request
import Servant.GitHub.Security (doesSignatureMatch, getToken, parseSignature)

-- | A combinator for matching incoming requests with a given GitHub event.
--
-- The handler function will be passed an 'Object' containing the data sent with the event.
-- Automatically verifies that the request is signed by GitHub.
--
-- Usage:
--
-- > import Servant.GitHub
-- >
-- > type MyApi = "webhook" :>
-- >   (    GitHubEvent 'InstallationEvent :> GitHubAction
-- >   :<|> GitHubEvent 'PushEvent :> GitHubAction
-- >   )
-- >
-- > handleInstallationEvent :: Object InstallationEvent -> Handler ()
-- > handlePushEvent :: Object PushEvent -> Handler ()
data GitHubEvent (event :: GitHubEventType)

instance
  ( HasServer api context
  , HasContextEntry context GitHubAppParams
  , IsGitHubEvent event
  , IsSchemaObject (EventSchema event)
  ) => HasServer (GitHubEvent event :> api) context where

  type ServerT (GitHubEvent event :> api) m = Object (EventSchema event) -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  route _ context = route (Proxy @api) context . addBodyCheck parseBody . addHeaderCheck_ verify
    where
      GitHubAppParams{ghWebhookSecret} = getContextEntry context

      verify request = do
        ghEvent <- getGitHubEvent request
        let e = ByteStringL.fromStrict ghEvent
        unless (e == eventName @event) $ Servant.delayedFail $ wrongEvent e

      parseBody request = do
        signature <- getSignature request
        ghSignature <- maybe (Servant.delayedFailFatal invalidSignature) return $ parseSignature signature
        body <- liftIO $ getRequestBody' request signature

        unless (doesSignatureMatch ghWebhookSecret (ByteStringL.toStrict body) ghSignature) $
          Servant.delayedFailFatal badSignature

        decodeRequestBody @(EventSchema event) body

      wrongEvent e = err400 { errBody = "Unhandled event: " <> e }
      invalidSignature = err401 { errBody = "Invalid signature found" }
      badSignature = err401 { errBody = "Signature did not match payload" }

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
type WithToken = WithToken' 'SingleToken

data TokenType
  = SingleToken
    -- ^ Generates a single token to use
  | TokenMaker
    -- ^ Provides a value @IO Token@ that generates a token. Useful for hooks that might take a
    -- long time and/or you want finer-grained control over token generation

data WithToken' (tokenType :: TokenType)

-- Generates a token that expires in the given number of minutes
type TokenGenerator = IO Token

mkTokenGenerator :: HasContextEntry context GitHubAppParams
  => Context context -> Request -> Servant.DelayedIO TokenGenerator
mkTokenGenerator context request = do
  event <- decodeRequestBody @BaseEvent =<< getRequestBody request
  return $ getToken ghSigner ghUserAgent ghAppId [get| event.installation!.id |]
  where
    GitHubAppParams{ghAppId, ghSigner, ghUserAgent} = getContextEntry context

instance
  ( HasServer api context
  , HasContextEntry context GitHubAppParams
  ) => HasServer (WithToken' 'SingleToken :> api) context where

  type ServerT (WithToken' 'SingleToken :> api) m = Token -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  -- TODO: fix the fact that a token is generated for every branch, even if the event didn't match
  route _ context = route (Proxy @api) context . addBodyCheck provideToken
    where
      provideToken = mkTokenGenerator context >=> liftIO

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

{- Helpers -}

decodeRequestBody :: forall (schema :: SchemaType). IsSchemaObject schema
  => ByteStringL.ByteString -> Servant.DelayedIO (Object schema)
decodeRequestBody = either fail return . eitherDecode @(Object schema)
