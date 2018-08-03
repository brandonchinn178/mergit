{-|
Module      :  MergeBot.Monad.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines monad definitions for the GitHub API.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Monad.GitHub
  ( GitHubT
  , runGitHubT
  , fromGitHub
  , toGitHub
  ) where

import Control.Lens (view, (&), (.~), (?~))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (Value(..), eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Wreq
    (Options, Response, auth, basicAuth, defaults, header, responseBody)
import Network.Wreq.Session (Session, getWith, newSession, postWith)
import System.Environment (getEnv)

import MergeBot.Monad.Class

data GitHubConfig = GitHubConfig
  { ghUsername :: Text
  , ghPassword :: Text
  , ghSession  :: Session
  }

newtype GitHubT m a = GitHubT { unGitHubT :: ReaderT GitHubConfig m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader GitHubConfig
    )

-- | Run GitHub actions.
runGitHubT :: MonadIO m => GitHubT m a -> m a
runGitHubT m = do
  -- TODO: take credentials in as arguments to runGitHubT
  ghUsername <- getEnv' "MERGEBOT_USERNAME"
  ghPassword <- getEnv' "MERGEBOT_PASSWORD"
  ghSession <- liftIO newSession
  runReaderT (unGitHubT m) GitHubConfig{..}
  where
    getEnv' = liftIO . fmap Text.pack . getEnv

instance MonadIO m => MonadGHBranch (GitHubT m) where
  createBranch _ = GitHubT $ liftIO $ putStrLn "createBranch"
  deleteBranch _ = GitHubT $ liftIO $ putStrLn "deleteBranch"
  mergeBranches _ _ = GitHubT $ liftIO $ putStrLn "mergeBranches"

instance MonadIO m => MonadGHPullRequest (GitHubT m) where
  mergePullRequest _ _ = GitHubT $ liftIO $ putStrLn "mergePullRequest"

{- Connecting to the GitHub API directly -}

-- | The GitHub API endpoint with question marks that can be replaced by passed-in values.
type Endpoint = Text

-- | Sends a GET request to the GitHub API.
fromGitHub :: MonadIO m => Endpoint -> [Text] -> GitHubT m Value
fromGitHub = githubAPI getWith

-- | Sends a POST request to the GitHub API.
toGitHub :: MonadIO m => Endpoint -> [Text] -> Value -> GitHubT m Value
toGitHub endpoint values postData = githubAPI postWith' endpoint values
  where
    postWith' opts session url = postWith opts session url postData

{- Helpers -}

-- | Interpolate the given values into the given endpoint with question marks.
populateEndpoint :: Endpoint -> [Text] -> Either String Text
populateEndpoint endpoint values =
  if length split == length values + 1
    then Right . Text.concat $ interleave ("":values) split
    else Left $ "Number of question marks mismatches number of arguments: " ++ Text.unpack endpoint
  where
    split = Text.splitOn "?" endpoint
    interleave xs ys = concat $ zipWith (\a b -> [a, b]) xs ys

-- | A helper function to connect to the GitHub API.
githubAPI :: MonadIO m
  => (Options -> Session -> String -> IO (Response ByteString))
  -> Endpoint
  -> [Text]
  -> GitHubT m Value
githubAPI httpWith endpoint values = do
  GitHubConfig{..} <- ask
  let opts' = opts & auth ?~ basicAuth (Text.encodeUtf8 ghUsername) (Text.encodeUtf8 ghPassword)

  endpoint' <- either fail return $ populateEndpoint endpoint values
  liftIO $ decode' . view responseBody =<< httpWith opts' ghSession (url ++ Text.unpack endpoint')
  where
    decode' = either fail return . eitherDecode
    opts = defaults
      & header "Accept" .~ ["application/vnd.github.v3+json"]
    url = "https://api.github.com"
