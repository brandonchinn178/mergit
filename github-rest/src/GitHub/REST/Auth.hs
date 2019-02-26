{-|
Module      :  GitHub.REST.Auth
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for handling authentication with the GitHub REST API.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.REST.Auth
  ( Token(..)
  , fromToken
  ) where

import Data.ByteString (ByteString)

-- | The token to use to authenticate with GitHub.
data Token
  = AccessToken ByteString
    -- ^ https://developer.github.com/v3/#authentication
  | BearerToken ByteString
    -- ^ https://developer.github.com/apps/building-github-apps/authenticating-with-github-apps/#authenticating-as-a-github-app
  deriving (Show)

fromToken :: Token -> ByteString
fromToken = \case
  AccessToken t -> "token " <> t
  BearerToken t -> "bearer " <> t
