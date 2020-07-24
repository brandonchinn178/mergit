{-|
Module      :  MergeBot.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines functions and types for authenticating in the MergeBot.
-}
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

module MergeBot.Auth
  ( AuthParams(..)
  , loadAuthParams
  , authCookieSettings
  , UserToken(..)
  , fromUserToken
  , redirectToLogin
  , XsrfToken
  , XsrfProtected
  , xsrfTokenInputName
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (liftIO)
import Crypto.JWT (fromRSA)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.X509 (PrivKey(..))
import Data.X509.File (readKeyFile)
import GitHub.REST (Token(..))
import Network.HTTP.Types (hContentType, hCookie, hLocation, methodGet)
import Network.Wai (lazyRequestBody, requestHeaders, requestMethod)
import Servant
import Servant.Auth.Server
import qualified Servant.Auth.Server.Internal.AddSetCookie as Servant.Auth
import Servant.Server (ServantErr)
import qualified Servant.Server.Internal as Servant
import System.Environment (getEnv, lookupEnv)
import Web.Cookie (parseCookies, setCookieValue)
import Web.FormUrlEncoded (urlDecodeAsForm)

{- GitHub app environment variables -}

data AuthParams = AuthParams
  { cookieSettings :: CookieSettings
  , jwtSettings    :: JWTSettings
  , ghClientId     :: String
  , ghClientSecret :: String
  , ghBaseUrl      :: String
  }

loadAuthParams :: IO AuthParams
loadAuthParams = do
  jwkFile <- fromMaybe "conf/cookie-jwk.pem" <$> lookupEnv "COOKIE_JWK"
  jwk <- readKeyFile jwkFile >>= \case
    PrivKeyRSA key:_ -> return $ fromRSA key
    _ -> fail $ "RSA key not found in key file: " ++ jwkFile

  let cookieSettings = authCookieSettings
        { cookieXsrfSetting = Nothing -- turn off XSRF for servant-auth; handle manually in XsrfProtected
        }
      jwtSettings = defaultJWTSettings jwk

  ghClientId <- getEnv "GITHUB_CLIENT_ID"
  ghClientSecret <- getEnv "GITHUB_CLIENT_SECRET"
  ghBaseUrl <- fromMaybe "http://localhost:3000" <$> lookupEnv "MERGE_BOT_URL"

  return AuthParams{..}

authCookieSettings :: CookieSettings
authCookieSettings = defaultCookieSettings
  { cookieIsSecure = NotSecure
  , sessionCookieName = "merge-bot-github-token"
  , cookieXsrfSetting = Just def
      { xsrfCookieName = xsrfTokenCookieName
      , xsrfExcludeGet = True
      }
  }

{- Authentication types + functions -}

newtype UserToken = UserToken Text
  deriving (Show,FromJSON,ToJSON)

instance FromJWT UserToken
instance ToJWT UserToken

fromUserToken :: UserToken -> Token
fromUserToken (UserToken token) = AccessToken $ Text.encodeUtf8 token

{- Redirection -}

-- TODO: pass in referer url to redirect post-login
redirectToLogin :: MonadError ServantErr m => AuthParams -> m a
redirectToLogin authParams = throwError $ err302 { errHeaders = [(hLocation, redirectUrl)] }
  where
    redirectUrl = Char8.pack $ ghBaseUrl authParams <> "/auth/login"

{- XSRF token -}

type XsrfToken = Text

xsrfTokenInputName :: Text
xsrfTokenInputName = "xsrfToken"

xsrfTokenCookieName :: ByteString
xsrfTokenCookieName = "merge-bot-xsrf-token"

-- | Handle XSRF protection. This combinator does the following actions:
--
-- * If this is a non-GET request, get the value of the 'xsrfTokenInputName' key in the body and
--   verify that it matches the current XSRF-TOKEN cookie
--
-- * Create and set a new XSRF-TOKEN token to use in this request and set as a cookie.
--
-- Doing this manually instead of using servant-auth because we want to check the XSRF token in the
-- POST body, not as a header.
data XsrfProtected

-- Copied a lot of magic from servant-auth-server
instance
  ( n ~ 'Servant.Auth.S 'Servant.Auth.Z
  , apiWithCookies ~ Servant.Auth.AddSetCookiesApi n api
  , HasServer api context
  , HasServer apiWithCookies context
  , Servant.Auth.AddSetCookies n (ServerT api Handler) (ServerT apiWithCookies Handler)
  ) => HasServer (XsrfProtected :> api) context where
  type ServerT (XsrfProtected :> api) m = XsrfToken -> ServerT api m

  hoistServerWithContext _ _ f s = hoistServerWithContext (Proxy @api) (Proxy @context) f . s

  route _ context subserver = route apiProxy context $ Servant.addAuthCheck (fmap go subserver) authCheck
    where
      apiProxy = Proxy @(Servant.Auth.AddSetCookiesApi n api)

      authCheck = Servant.withRequest $ \req -> do
        checkXsrf req
        liftIO $ makeXsrfCookie authCookieSettings

      checkXsrf req
        | requestMethod req == methodGet = return ()

        | Just xsrfTokenCookie <- getXsrfTokenCookie req
        , getHeader hContentType req == Just "application/x-www-form-urlencoded"
        = do
          body <- liftIO $ lazyRequestBody req
          bodyData <- case urlDecodeAsForm body of
            Right bodyData -> return bodyData
            Left e -> error $ "could not decode body: " ++ Text.unpack e

          case lookup xsrfTokenInputName bodyData of
            Just xsrfTokenValue | xsrfTokenValue == xsrfTokenCookie -> return ()
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
