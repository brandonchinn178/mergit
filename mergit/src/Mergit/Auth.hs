{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      :  Mergit.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions and types for authenticating in Mergit.
-}
module Mergit.Auth (
  AuthParams (..),
  loadAuthParams,
  authCookieSettings,
  UserToken (..),
  fromUserToken,
  redirectToLogin,
  XsrfToken,
  XsrfProtected,
  xsrfTokenInputName,
) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Crypto.JWT (fromRSA)
import Crypto.PubKey.RSA qualified as Crypto
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import GitHub.REST (Token (..))
import Network.HTTP.Types (hContentType, hCookie, hLocation, methodGet)
import Network.Wai (lazyRequestBody, requestHeaders, requestMethod)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.Internal.AddSetCookie qualified as Servant.Auth
import Servant.Server.Internal qualified as Servant
import System.Environment (getEnv, lookupEnv)
import Web.Cookie (parseCookies, setCookieValue)
import Web.FormUrlEncoded (urlDecodeAsForm)

{- GitHub app environment variables -}

data AuthParams = AuthParams
  { cookieSettings :: CookieSettings
  , jwtSettings :: JWTSettings
  , ghClientId :: String
  , ghClientSecret :: String
  , ghBaseUrl :: String
  }

loadAuthParams :: IO AuthParams
loadAuthParams = do
  jwk <-
    lookupEnv "COOKIE_JWK" >>= \case
      Just jwkFile ->
        readKeyFile jwkFile >>= \case
          PrivKeyRSA key : _ -> pure $ fromRSA key
          _ -> fail $ "RSA key not found in key file: " ++ jwkFile
      Nothing -> do
        -- if COOKIE_JWK is not set, generate a random new key
        -- (this key is rather insecure, but this should only happen in development)
        putStrLn "Generating random COOKIE_JWK..."
        (_, key) <- Crypto.generate 256 3
        putStrLn "Done."
        pure $ fromRSA key

  let cookieSettings =
        authCookieSettings
          { cookieXsrfSetting = Nothing -- turn off XSRF for servant-auth; handle manually in XsrfProtected
          }
      jwtSettings = defaultJWTSettings jwk

  ghClientId <- getEnv "GITHUB_CLIENT_ID"
  ghClientSecret <- getEnv "GITHUB_CLIENT_SECRET"
  ghBaseUrl <- fromMaybe "http://localhost:3000" <$> lookupEnv "MERGIT_URL"

  pure AuthParams{..}

authCookieSettings :: CookieSettings
authCookieSettings =
  defaultCookieSettings
    { cookieIsSecure = NotSecure
    , sessionCookieName = "mergit-github-token"
    , cookieXsrfSetting =
        Just
          def
            { xsrfCookieName = xsrfTokenCookieName
            , xsrfExcludeGet = True
            }
    }

{- Authentication types + functions -}

newtype UserToken = UserToken Text
  deriving (Show, FromJSON, ToJSON)

instance FromJWT UserToken
instance ToJWT UserToken

fromUserToken :: UserToken -> Token
fromUserToken (UserToken token) = AccessToken $ Text.encodeUtf8 token

{- Redirection -}

-- TODO: pass in referer url to redirect post-login
redirectToLogin :: MonadError ServerError m => AuthParams -> m a
redirectToLogin authParams = throwError $ err302{errHeaders = [(hLocation, redirectUrl)]}
  where
    redirectUrl = Char8.pack $ ghBaseUrl authParams <> "/auth/login"

{- XSRF token -}

type XsrfToken = Text

xsrfTokenInputName :: Text
xsrfTokenInputName = "xsrfToken"

xsrfTokenCookieName :: ByteString
xsrfTokenCookieName = "mergit-xsrf-token"

-- | Handle XSRF protection. This combinator does the following actions:
--
--  * If this is a non-GET request, get the value of the 'xsrfTokenInputName' key in the body and
--    verify that it matches the current XSRF-TOKEN cookie
--
--  * Create and set a new XSRF-TOKEN token to use in this request and set as a cookie.
--
--  Doing this manually instead of using servant-auth because we want to check the XSRF token in the
--  POST body, not as a header.
data XsrfProtected

-- Copied a lot of magic from servant-auth-server
instance
  ( n ~ 'Servant.Auth.S 'Servant.Auth.Z
  , apiWithCookies ~ Servant.Auth.AddSetCookiesApi n api
  , HasServer api context
  , HasServer apiWithCookies context
  , Servant.Auth.AddSetCookies n (ServerT api Handler) (ServerT apiWithCookies Handler)
  ) =>
  HasServer (XsrfProtected :> api) context
  where
  type ServerT (XsrfProtected :> api) m = XsrfToken -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  route _ context subserver = route apiProxy context $ Servant.addAuthCheck (fmap go subserver) authCheck
    where
      apiProxy = Proxy @(Servant.Auth.AddSetCookiesApi n api)

      authCheck = Servant.withRequest $ \req -> do
        checkXsrf req
        liftIO $ makeXsrfCookie authCookieSettings

      checkXsrf req
        | requestMethod req == methodGet = pure ()
        | Just xsrfTokenCookie <- getXsrfTokenCookie req
        , getHeader hContentType req == Just "application/x-www-form-urlencoded" =
            do
              body <- liftIO $ lazyRequestBody req
              bodyData <- case urlDecodeAsForm body of
                Right bodyData -> pure bodyData
                Left e -> error $ "could not decode body: " ++ Text.unpack e

              case lookup xsrfTokenInputName bodyData of
                Just xsrfTokenValue | xsrfTokenValue == xsrfTokenCookie -> pure ()
                _ -> Servant.delayedFail Servant.err401

        -- good for now, revisit if we need to handle these cases
        | otherwise = error "Non-GET requests must use application/x-www-form-urlencoded"

      go f xsrfTokenSetCookie =
        let setCookieList = Just xsrfTokenSetCookie `Servant.Auth.SetCookieCons` Servant.Auth.SetCookieNil
            xsrfToken = Text.decodeUtf8 $ setCookieValue xsrfTokenSetCookie
         in Servant.Auth.addSetCookies setCookieList $ f xsrfToken

      getXsrfTokenCookie req =
        let cookies = parseCookies . fromMaybe "" . getHeader hCookie $ req
         in Text.decodeUtf8 <$> lookup xsrfTokenCookieName cookies

      getHeader headerName = lookup headerName . requestHeaders
