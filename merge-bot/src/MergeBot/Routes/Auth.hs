{-|
Module      :  MergeBot.Routes.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines authentication routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
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

import Control.Monad.IO.Class (liftIO)
import Crypto.JWT (fromRSA)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.=), (.:))
import qualified Data.ByteString.Char8 as Char8
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.X509 (PrivKey(..))
import Data.X509.File (readKeyFile)
import GitHub.REST (Token(..))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (hLocation, renderSimpleQuery)
import Servant
import Servant.Auth.Server
import Servant.HTML.Blaze (HTML)
import Servant.Server (ServantErr)
import System.Environment (getEnv, lookupEnv)

type AuthRoutes =
  "login" :> LoginRoute
  :<|> "callback" :> CallbackRoute

handleAuthRoutes :: AuthParams -> Server AuthRoutes
handleAuthRoutes authParams = handleLoginRoute authParams :<|> handleCallbackRoute authParams

type LoginRoute = Redirect '[HTML] (RedirectResult '[])

handleLoginRoute :: AuthParams -> Handler (RedirectResult '[])
handleLoginRoute AuthParams{..} = do
  let redirectUrl = "https://github.com/login/oauth/authorize" <> renderSimpleQuery True
        [ ("client_id", Char8.pack ghClientId)
        , ("redirect_uri", Char8.pack $ ghBaseUrl <> "/auth/callback")
        ]
  return $ addHeader (Char8.unpack redirectUrl) NoContent

type SetCookieHeaders = '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
type CallbackRoute =
  QueryParam' '[Required, Strict] "code" String :>
  Redirect '[HTML] (RedirectResult SetCookieHeaders)

handleCallbackRoute :: AuthParams -> String -> Handler (RedirectResult SetCookieHeaders)
handleCallbackRoute AuthParams{..} ghCode = do
  token <- liftIO $ getAccessToken AccessTokenRequest{..}

  -- TODO: use referer url
  let redirectUrl = ghBaseUrl

  liftIO $ acceptLogin cookieSettings jwtSettings token >>= \case
    Nothing -> fail "Could not make JWT"
    Just addCookieHeaders -> return $ addHeader redirectUrl $ addCookieHeaders NoContent

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
        , cookieXsrfSetting = Just def { xsrfExcludeGet = True }
        }
      jwtSettings = defaultJWTSettings jwk

  ghClientId <- getEnv "GITHUB_CLIENT_ID"
  ghClientSecret <- getEnv "GITHUB_CLIENT_SECRET"
  ghBaseUrl <- fromMaybe "http://localhost:3000" <$> lookupEnv "MERGE_BOT_URL"

  return AuthParams{..}

{- GitHub access token -}

data AccessTokenRequest = AccessTokenRequest
  { ghClientId     :: String
  , ghClientSecret :: String
  , ghCode         :: String
  }

instance ToJSON AccessTokenRequest where
  toJSON AccessTokenRequest{..} = object
    [ "client_id" .= ghClientId
    , "client_secret" .= ghClientSecret
    , "code" .= ghCode
    ]

data AccessTokenResponse = AccessTokenResponse
  { accessToken :: String
  }

instance FromJSON AccessTokenResponse where
  parseJSON = withObject "AccessTokenResponse" $ \o ->
    AccessTokenResponse <$> o .: "access_token"

getAccessToken :: AccessTokenRequest -> IO UserToken
getAccessToken reqBody = Req.runReq def $ do
  let githubUrl = Req.https "github.com" /: "login" /: "oauth" /: "access_token"
      acceptHeader = Req.header "Accept" "application/json"

  resp <- Req.req Req.POST githubUrl (Req.ReqBodyJson reqBody) Req.jsonResponse acceptHeader
  return . UserToken . Text.pack . accessToken . Req.responseBody $ resp

{- Authentication types + functions -}

newtype UserToken = UserToken Text
  deriving (FromJSON,ToJSON)

instance FromJWT UserToken
instance ToJWT UserToken

fromUserToken :: UserToken -> Token
fromUserToken (UserToken token) = AccessToken $ Text.encodeUtf8 token

{- Redirection -}

type RedirectResult (hdrs :: [*]) = Headers (Header "Location" String ': hdrs) NoContent
type Redirect = Verb 'GET 302

-- TODO: pass in referer url to redirect post-login
redirectToLogin :: AuthParams -> ServantErr
redirectToLogin authParams = err302 { errHeaders = [(hLocation, redirectUrl)] }
  where
    redirectUrl = Char8.pack $ ghBaseUrl authParams <> "/auth/login"
