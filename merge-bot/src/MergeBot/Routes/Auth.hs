{-|
Module      :  MergeBot.Routes.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines authentication routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Auth
  ( AuthRoutes
  , handleAuthRoutes
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import Network.HTTP.Client
    ( Request(..)
    , RequestBody(..)
    , Response(..)
    , httpLbs
    , newManager
    , parseRequest_
    , throwErrorStatusCodes
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAccept, hContentType, renderSimpleQuery)
import Servant
import Servant.Auth.Server (SetCookie, acceptLogin, makeXsrfCookie)
import Servant.HTML.Blaze (HTML)

import MergeBot.Auth (AuthParams(..), UserToken(..), authCookieSettings)
import MergeBot.Monad (BaseApp, ServerBase, getAuthParams)

type AuthRoutes =
  "login" :> LoginRoute
  :<|> "callback" :> CallbackRoute

handleAuthRoutes :: ServerBase AuthRoutes
handleAuthRoutes = handleLoginRoute :<|> handleCallbackRoute

type LoginRoute = Redirect '[HTML] (RedirectResult '[])

handleLoginRoute :: BaseApp (RedirectResult '[])
handleLoginRoute = do
  AuthParams{..} <- getAuthParams

  let redirectUrl = "https://github.com/login/oauth/authorize" <> renderSimpleQuery True
        [ ("client_id", Char8.pack ghClientId)
        , ("redirect_uri", Char8.pack $ ghBaseUrl <> "/auth/callback")
        ]
  return $ addHeader (Char8.unpack redirectUrl) NoContent

type SetCookieHeaders =
  '[ Header "Set-Cookie" SetCookie -- JWT token from servant-auth
   , Header "Set-Cookie" SetCookie -- nonexistent XSRF token from servant-auth
   , Header "Set-Cookie" SetCookie -- XSRF token we're manually generating
   ]
type CallbackRoute =
  QueryParam' '[Required, Strict] "code" String :>
  Redirect '[HTML] (RedirectResult SetCookieHeaders)

handleCallbackRoute :: String -> BaseApp (RedirectResult SetCookieHeaders)
handleCallbackRoute ghCode = do
  AuthParams{..} <- getAuthParams
  token <- liftIO $ getAccessToken AccessTokenRequest{..}

  -- TODO: use referer url
  let redirectUrl = ghBaseUrl

  liftIO $ acceptLogin cookieSettings jwtSettings token >>= \case
    Nothing -> fail "Could not make JWT"
    Just addCookieHeaders -> do
      xsrfCookie <- makeXsrfCookie authCookieSettings
      return $ addHeader redirectUrl $ addHeader xsrfCookie $ addCookieHeaders NoContent

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
getAccessToken reqBody = do
  manager <- newManager tlsManagerSettings
  let request = (parseRequest_ "https://github.com/login/oauth/access_token")
        { method = "POST"
        , requestHeaders =
            [ (hAccept, "application/json")
            , (hContentType, "application/json")
            ]
        , requestBody = RequestBodyLBS $ encode reqBody
        , checkResponse = throwErrorStatusCodes
        }

  response <- httpLbs request manager
  either fail (return . UserToken . Text.pack . accessToken) $
    eitherDecode $ responseBody response

{- Redirection -}

type RedirectResult (hdrs :: [*]) = Headers (Header "Location" String ': hdrs) NoContent
type Redirect = Verb 'GET 302
