{-|
Module      :  MergeBot.Monad.GitHub
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines monad definitions for the GitHub API.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Monad.GitHub
  ( GitHubT
  , runGitHubT
  , GitHubData(..)
  , getGitHub
  , postGitHub
  , deleteGitHub
  ) where

import Control.Lens (view, (&), (.~), (?~))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson
    (FromJSON, ToJSON(..), Value(..), eitherDecode, object, withObject)
import Data.Aeson.Types (parseEither, parseField)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Wreq
    (Options, Response, auth, basicAuth, defaults, header, responseBody)
import Network.Wreq.Session (Session, deleteWith, getWith, newSession, postWith)
import System.Environment (getEnv)

import MergeBot.Monad.Class

data GitHubConfig = GitHubConfig
  { ghOwner    :: Text
  , ghRepo     :: Text
  , ghUsername :: Text
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
  -- TODO: take owner/repo/credentials in as arguments to runGitHubT
  let ghOwner = "brandon-leapyear"
      ghRepo = "merge-bot-test"
  ghUsername <- getEnv' "MERGEBOT_USERNAME"
  ghPassword <- getEnv' "MERGEBOT_PASSWORD"
  ghSession <- liftIO newSession
  runReaderT (unGitHubT m) GitHubConfig{..}
  where
    getEnv' = liftIO . fmap Text.pack . getEnv

instance MonadIO m => MonadGHBranch (GitHubT m) where
  createBranch branch = do
    GitHubConfig{..} <- ask
    master <- getGitHub "/repos/:owner/:repo/git/refs/:ref" [ghOwner, ghRepo, "heads/master"]
    let sha = master .: "object" .: "sha" :: Text
    postGitHub "/repos/:owner/:repo/git/refs" [ghOwner, ghRepo]
      [ "ref" := "refs/heads/" <> branch
      , "sha" := sha
      ]

  deleteBranch branch = do
    GitHubConfig{..} <- ask
    deleteGitHub "/repos/:owner/:repo/git/refs/:ref" [ghOwner, ghRepo, "heads/" <> branch]

  mergeBranches _ _ = GitHubT $ liftIO $ putStrLn "mergeBranches"

instance MonadIO m => MonadGHPullRequest (GitHubT m) where
  mergePullRequest _ _ = GitHubT $ liftIO $ putStrLn "mergePullRequest"

{- Connecting to the GitHub API directly -}

-- | The GitHub API endpoint with placeholders of the form ":abc" that can be replaced by
-- passed-in values.
type Endpoint = Text

-- | Sends a GET request to the GitHub API.
getGitHub :: MonadIO m => Endpoint -> [Text] -> GitHubT m Value
getGitHub = githubAPI getWith

-- | Sends a POST request to the GitHub API.
postGitHub :: MonadIO m => Endpoint -> [Text] -> [GitHubData] -> GitHubT m ()
postGitHub endpoint values postData = void $ githubAPI postWith' endpoint values
  where
    postWith' opts session url = postWith opts session url $ fromData postData

-- | Sends a DELETE request to the GitHub API.
deleteGitHub :: MonadIO m => Endpoint -> [Text] -> GitHubT m ()
deleteGitHub endpoint = void . githubAPI deleteWith endpoint

{- API Helpers -}

-- | Set the placeholders in the given endpoint to the given values.
populateEndpoint :: Endpoint -> [Text] -> Text
populateEndpoint endpoint values =
  Text.intercalate "/" $ populate (Text.splitOn "/" endpoint) values
  where
    populate [] [] = []
    populate [] _ = fail' "Too many values passed into endpoint"
    populate (x:xs) vs =
      if ":" `Text.isPrefixOf` x
        then case vs of
          [] -> fail' "Not enough values passed into endpoint"
          v:vs' -> v : populate xs vs'
        else x : populate xs vs
    fail' msg = error $ Text.unpack $ msg <> ": " <> endpoint

-- | A helper function to connect to the GitHub API.
githubAPI :: MonadIO m
  => (Options -> Session -> String -> IO (Response ByteString))
  -> Endpoint
  -> [Text]
  -> GitHubT m Value
githubAPI httpWith endpoint values = do
  GitHubConfig{..} <- ask
  let opts' = opts & auth ?~ basicAuth (Text.encodeUtf8 ghUsername) (Text.encodeUtf8 ghPassword)

  response <- liftIO $ view responseBody <$> httpWith opts' ghSession url
  if ByteString.null response
    then return $ object []
    else decode' response
  where
    decode' = either fail return . eitherDecode
    opts = defaults
      & header "Accept" .~ ["application/vnd.github.v3+json"]
    url = Text.unpack $ "https://api.github.com" <> populateEndpoint endpoint values

{- JSON helpers -}

-- | Data key-value pairs that can be sent to GitHub.
data GitHubData where
  (:=) :: ToJSON v => Text -> v -> GitHubData
infixl 1 :=

-- | Convert the given GitHubData into a JSON value to send to GitHub.
fromData :: [GitHubData] -> Value
fromData = object . map (\(k := v) -> (k, toJSON v))

(.:) :: FromJSON a => Value -> Text -> a
(.:) v key = either error id $ parseEither parseObject v
  where
    parseObject = withObject "parseObject" (`parseField` key)
