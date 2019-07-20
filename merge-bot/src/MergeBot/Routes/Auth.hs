{-|
Module      :  MergeBot.Routes.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines authentication routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Auth
  ( -- * Routes
    AuthRoutes
  , handleAuthRoutes
    -- * Authentication helpers
  , AuthParams(..)
  , loadAuthParams
  , UserToken
  , fromUserToken
  , redirectToLogin
  ) where

import Crypto.JWT (fromRSA)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.X509 (PrivKey(..))
import Data.X509.File (readKeyFile)
import GitHub.REST (Token(..))
import Network.HTTP.Types (hLocation)
import Servant
import Servant.Auth.Server (CookieSettings(..), JWTSettings, FromJWT, ToJWT, defaultCookieSettings, defaultJWTSettings)
import Servant.Server (ServantErr)
import System.Environment (getEnv, lookupEnv)

type AuthRoutes =
  "login" :> LoginRoute
  :<|> "callback" :> CallbackRoute

handleAuthRoutes :: AuthParams -> Server AuthRoutes
handleAuthRoutes authParams = handleLoginRoute authParams :<|> handleCallbackRoute authParams

type LoginRoute = Get '[JSON] () -- TODO: Redirect

handleLoginRoute :: AuthParams -> Handler ()
handleLoginRoute _ =
  return ()

type CallbackRoute = Get '[JSON] () -- TODO: Redirect

handleCallbackRoute :: AuthParams -> Handler ()
handleCallbackRoute _ =
  return ()

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
    (PrivKeyRSA key):_ -> return $ fromRSA key
    _ -> fail $ "RSA key not found in key file: " ++ jwkFile

  let cookieSettings = defaultCookieSettings
        { cookieIsSecure = NotSecure
        , sessionCookieName = "merge-bot-github-token"
        }
      jwtSettings = defaultJWTSettings jwk

  ghClientId <- getEnv "GITHUB_CLIENT_ID"
  ghClientSecret <- getEnv "GITHUB_CLIENT_SECRET"
  ghBaseUrl <- fromMaybe "http://localhost:3000" <$> lookupEnv "MERGE_BOT_URL"

  return AuthParams{..}

{- Authentication types + functions -}

newtype UserToken = UserToken Text
  deriving (FromJSON,ToJSON)

instance FromJWT UserToken
instance ToJWT UserToken

fromUserToken :: UserToken -> Token
fromUserToken (UserToken token) = AccessToken $ Text.encodeUtf8 token

-- TODO: pass in referer url to redirect post-login
redirectToLogin :: AuthParams -> ServantErr
redirectToLogin authParams = err302 { errHeaders = [(hLocation, redirectUrl)] }
  where
    redirectUrl = Char8.pack $ ghBaseUrl authParams <> "/auth/login"
