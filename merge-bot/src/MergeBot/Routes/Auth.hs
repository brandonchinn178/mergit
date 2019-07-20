{-|
Module      :  MergeBot.Routes.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines authentication routes for the MergeBot.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Auth
  ( -- * Routes
    AuthRoutes
  , handleAuthRoutes
    -- * Authentication helpers
  , UserToken
  , fromUserToken
  , redirectToLogin
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import GitHub.REST (Token(..))
import Network.HTTP.Types (hLocation)
import Servant
import Servant.Auth.Server (FromJWT, ToJWT)
import Servant.Server (ServantErr)
import System.Environment (getEnv)

type AuthRoutes =
  "login" :> LoginRoute
  :<|> "callback" :> CallbackRoute

handleAuthRoutes :: Server AuthRoutes
handleAuthRoutes = handleLoginRoute :<|> handleCallbackRoute

type LoginRoute = Get '[JSON] () -- TODO: Redirect

handleLoginRoute :: Handler ()
handleLoginRoute = do
  _ <- liftIO loadGitHubClientInfo
  return ()

type CallbackRoute = Get '[JSON] () -- TODO: Redirect

handleCallbackRoute :: Handler ()
handleCallbackRoute = do
  _ <- liftIO loadGitHubClientInfo
  return ()

{- GitHub app environment variables -}

data GitHubClientInfo = GitHubClientInfo
  { ghClientId     :: String
  , ghClientSecret :: String
  }

loadGitHubClientInfo :: IO GitHubClientInfo
loadGitHubClientInfo = do
  ghClientId <- getEnv "GITHUB_CLIENT_ID"
  ghClientSecret <- getEnv "GITHUB_CLIENT_SECRET"
  return GitHubClientInfo{..}

{- Authentication types + functions -}

newtype UserToken = UserToken Text
  deriving (FromJSON,ToJSON)

instance FromJWT UserToken
instance ToJWT UserToken

fromUserToken :: UserToken -> Token
fromUserToken (UserToken token) = AccessToken $ Text.encodeUtf8 token

-- TODO: pass in referer url to redirect post-login
redirectToLogin :: ServantErr
redirectToLogin = err302 { errHeaders = [(hLocation, "/auth/login")] }
