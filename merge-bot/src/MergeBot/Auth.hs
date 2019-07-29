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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Auth
  ( AuthParams(..)
  , loadAuthParams
  , UserToken(..)
  , fromUserToken
  , redirectToLogin
  ) where

import Control.Monad.Except (MonadError(..))
import Crypto.JWT (fromRSA)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as Char8
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.X509 (PrivKey(..))
import Data.X509.File (readKeyFile)
import GitHub.REST (Token(..))
import Network.HTTP.Types (hLocation)
import Servant
import Servant.Auth.Server
import Servant.Server (ServantErr)
import System.Environment (getEnv, lookupEnv)

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
