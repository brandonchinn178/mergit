{-|
Module      :  MergeBot.Monad.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines monad definitions for the GitHub API.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Monad.GitHub
  ( GitHubT(..)
  , fromGitHub
  ) where

import Control.Lens (view, (&), (.~), (?~))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value, eitherDecode)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wreq (auth, basicAuth, defaults, getWith, header, responseBody)
import System.Environment (getEnv)

import MergeBot.Monad.Class

newtype GitHubT m a = GitHubT { runGitHubT :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadGHBranch (GitHubT m) where
  createBranch _ = GitHubT $ liftIO $ putStrLn "createBranch"
  deleteBranch _ = GitHubT $ liftIO $ putStrLn "deleteBranch"
  mergeBranches _ _ = GitHubT $ liftIO $ putStrLn "mergeBranches"

instance MonadIO m => MonadGHPullRequest (GitHubT m) where
  mergePullRequest _ _ = GitHubT $ liftIO $ putStrLn "mergePullRequest"

-- | The GitHub API endpoint with question marks that can be replaced by passed-in values.
type Endpoint = Text

-- | Interpolate the given values into the given endpoint with question marks.
populateEndpoint :: Endpoint -> [Text] -> Either String Text
populateEndpoint endpoint values =
  if length split == length values + 1
    then Right . Text.concat $ interleave ("":values) split
    else Left $ "Number of question marks mismatches number of arguments: " ++ Text.unpack endpoint
  where
    split = Text.splitOn "?" endpoint
    interleave xs ys = concat $ zipWith (\a b -> [a, b]) xs ys

-- | Sends a GET request to the GitHub API.
fromGitHub :: MonadIO m => Endpoint -> [Text] -> m Value
fromGitHub endpoint values = liftIO $ do
  -- TODO: save auth in GitHubT state, use Wreq.Session to cache network
  user <- fromString <$> getEnv "USERNAME"
  pass <- fromString <$> getEnv "PASSWORD"
  let opts' = opts & auth ?~ basicAuth user pass

  endpoint' <- either fail return $ populateEndpoint endpoint values
  decode' . view responseBody =<< getWith opts' (url ++ Text.unpack endpoint')
  where
    decode' = either fail return . eitherDecode
    opts = defaults
      & header "Accept" .~ ["application/vnd.github.v3+json"]
    url = "https://api.github.com"
