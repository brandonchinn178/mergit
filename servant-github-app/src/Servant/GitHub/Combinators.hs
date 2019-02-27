{-|
Module      :  Servant.GitHub.Combinators
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Servant combinators for serving a GitHub App.
-}
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

module Servant.GitHub.Combinators
  ( GitHubSigned
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Proxy (Proxy(..))
import Network.Wai (lazyRequestBody, requestHeaders)
import Servant
import qualified Servant.Server.Internal as Servant

import Servant.GitHub.Context (GitHubAppParams(..))
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

  route _ context = route (Proxy @api) context . addBodyCheck (Servant.withRequest verify)
    where
      GitHubAppParams{ghWebhookSecret} = getContextEntry context

      verify request = do
        body <- liftIO $ ByteStringL.toStrict <$> lazyRequestBody request
        signature <- maybeDelayed invalidSignature . parseSignature =<<
          maybeDelayed missingSignature (lookup "x-hub-signature" $ requestHeaders request)

        unless (doesSignatureMatch ghWebhookSecret body signature)
          $ Servant.delayedFailFatal badSignature

      missingSignature = err400 { errBody = "x-hub-signature header not found"}
      invalidSignature = err401 { errBody = "Invalid signature found" }
      badSignature = err401 { errBody = "Signature did not match payload" }

{- Helpers -}

maybeDelayed :: ServantErr -> Maybe a -> Servant.DelayedIO a
maybeDelayed e = maybe (Servant.delayedFailFatal e) return

-- | The function I wish 'Servant.addBodyCheck' actually was.
addBodyCheck :: Servant.DelayedIO () -> Servant.Delayed env a -> Servant.Delayed env a
addBodyCheck bodyCheck Servant.Delayed{..} =
  Servant.Delayed { Servant.bodyD = \content -> bodyCheck >> bodyD content, .. }
