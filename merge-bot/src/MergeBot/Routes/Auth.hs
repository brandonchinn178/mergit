{-|
Module      :  MergeBot.Routes.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines authentication routes for the MergeBot.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Routes.Auth
  ( -- * Routes
    -- TODO
    -- * Authentication helpers
    UserToken
  , fromUserToken
  , redirectToLogin
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GitHub.REST (Token(..))
import Network.HTTP.Types (hLocation)
import Servant
import Servant.Auth.Server (FromJWT, ToJWT)
import Servant.Server (ServantErr)

newtype UserToken = UserToken Text
  deriving (FromJSON,ToJSON)

instance FromJWT UserToken
instance ToJWT UserToken

fromUserToken :: UserToken -> Token
fromUserToken (UserToken token) = AccessToken $ Text.encodeUtf8 token

redirectToLogin :: ServantErr
redirectToLogin = err302 { errHeaders = [(hLocation, "/auth/login")] }
