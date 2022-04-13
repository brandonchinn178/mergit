{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Servant.GitHub.Context
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions for loading 'GitHubAppParams' to use in the Servant context.
-}
module Servant.GitHub.Context (
  GitHubAppParams (..),
  loadGitHubAppParams,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import GitHub.REST.Auth (loadSigner)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import Web.JWT (EncodeSigner)

{- | Parameters loaded from the environment that specify parameters required to make GitHub App
 functionality work correctly and securely.
-}
data GitHubAppParams = GitHubAppParams
  { ghAppId :: Int
  , ghWebhookSecret :: ByteString
  , ghSigner :: EncodeSigner
  , ghUserAgent :: ByteString
  }

-- | Load 'GitHubAppParams' from environment variables.
loadGitHubAppParams :: IO GitHubAppParams
loadGitHubAppParams = do
  ghAppId <- parseInt =<< getEnv "GITHUB_APP_ID"
  ghWebhookSecret <- Char8.pack <$> getEnv "GITHUB_WEBHOOK_SECRET"
  ghSigner <- loadSigner =<< getEnv "GITHUB_PRIVATE_KEY"
  ghUserAgent <- Char8.pack <$> getEnv "GITHUB_USER_AGENT"

  return GitHubAppParams{..}

{- Helpers -}

parseInt :: Monad m => String -> m Int
parseInt x = maybe (error $ "Invalid int: " ++ x) return $ readMaybe x
