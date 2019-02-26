{-|
Module      :  Servant.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines Servant combinators for serving a GitHub App.
-}
{-# LANGUAGE RecordWildCards #-}

module Servant.GitHub
  ( loadGitHubAppParams
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import System.Environment (getEnv)
import Text.Read (readMaybe)
import Web.JWT (Signer)

import Servant.GitHub.Security

-- | Parameters loaded from the environment that specify parameters required to make GitHub App
-- functionality work correctly and securely.
data GitHubAppParams = GitHubAppParams
  { ghAppId         :: Int
  , ghWebhookSecret :: ByteString
  , ghSigner        :: Signer
  }

loadGitHubAppParams :: IO GitHubAppParams
loadGitHubAppParams = do
  ghAppId <- parseInt =<< getEnv "GITHUB_APP_ID"
  ghWebhookSecret <- Char8.pack <$> getEnv "GITHUB_WEBHOOK_SECRET"
  ghSigner <- loadSigner =<< getEnv "GITHUB_PRIVATE_KEY"

  return GitHubAppParams{..}

{- Helpers -}

parseInt :: Monad m => String -> m Int
parseInt x = maybe (fail $ "Invalid int: " ++ x) return $ readMaybe x
