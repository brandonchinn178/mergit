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
  , KeyValue(..)
  , (.:)
  -- * Low-level operations
  , github
  , github_
  , githubWith
  , githubWith_
  , handleStatus
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow, handleJust)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson
    (FromJSON, ToJSON(..), Value(..), eitherDecode, encode, object, withObject)
import Data.Aeson.Types (parseEither, parseField)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid ((<>))
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client
    ( HttpException(..)
    , HttpExceptionContent(..)
    , Manager
    , Request(..)
    , RequestBody(..)
    , applyBasicAuth
    , httpLbs
    , newManager
    , parseRequest_
    , responseBody
    , responseStatus
    , throwErrorStatusCodes
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
    ( Status
    , StdMethod(..)
    , hAccept
    , hUserAgent
    , renderStdMethod
    , status404
    )
import System.Environment (getEnv)

import MergeBot.Monad.Class

data GitHubConfig = GitHubConfig
  { ghOwner    :: Text
  , ghRepo     :: Text
  , ghUsername :: Text
  , ghPassword :: Text
  , ghManager  :: Manager
  }

newtype GitHubT m a = GitHubT { unGitHubT :: ReaderT GitHubConfig m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatch
    , MonadThrow
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
  ghManager <- liftIO $ newManager tlsManagerSettings
  runReaderT (unGitHubT m) GitHubConfig{..}
  where
    getEnv' = liftIO . fmap Text.pack . getEnv

instance (MonadCatch m, MonadIO m) => MonadGHBranch (GitHubT m) where
  getBranch patchId = fmap toRef $ handleStatus status404 $
    github GET "/repos/:owner/:repo/pulls/:number" ["number" :=* patchId]
    where
      toRef = either (const Nothing) (Just . getRef)
      getRef patch = patch .: "head" .: "ref"

  createBranch branch = do
    master <- github GET "/repos/:owner/:repo/git/refs/:ref" ["ref" := "heads/master"]
    githubWith_ POST "/repos/:owner/:repo/git/refs" []
      [ "ref" := "refs/heads/" <> branch
      , "sha" := master .: "object" .: "sha"
      ]

  deleteBranch branch =
    github_ DELETE "/repos/:owner/:repo/git/refs/:ref" ["ref" := "heads/" <> branch]

  mergeBranch base branch =
    githubWith_ POST "/repos/:owner/:repo/merges" []
      [ "base" := base
      , "head" := branch
      , "commit_message" := Text.unwords ["[lybot] Merge branch", branch, "into", base]
      ]

instance (MonadIO m, MonadCatch m) => MonadGHPullRequest (GitHubT m) where
  mergePullRequest patchId mergeAlgorithm = do
    patch <- github GET "/repos/:owner/:repo/pulls/:number" ["number" :=* patchId]
    githubWith_ PUT "/repos/:owner/:repo/pulls/:number/merge" ["number" :=* patchId]
      [ "commit_title" := "[lybot] Merge #" <> showT patchId
      , "commit_message" := patch .: "title"
      , "sha" := patch .: "head" .: "sha"
      , "merge_method" := showT mergeAlgorithm
      ]
    deleteBranch $ patch .: "head" .: "ref"

{- Connecting to the GitHub API directly -}

-- | The GitHub API endpoint with placeholders of the form ":abc" that can be replaced by
-- passed-in values.
type Endpoint = Text

-- | Mapping to populate the endpoint.
type EndpointVals = [KeyValue]

-- | Data to send to GitHub.
type GitHubData = [KeyValue]

-- | Access the GitHub API with the given method.
github :: MonadIO m => StdMethod -> Endpoint -> EndpointVals -> GitHubT m Value
github method endpoint vals = githubAPI Nothing method endpoint vals

-- | 'github' but ignoring the result.
github_ :: MonadIO m => StdMethod -> Endpoint -> EndpointVals -> GitHubT m ()
github_ method endpoint vals = void $ github method endpoint vals

-- | Access the GitHub API with the given method, sending data in the request body.
githubWith :: MonadIO m => StdMethod -> Endpoint -> EndpointVals -> GitHubData -> GitHubT m Value
githubWith method endpoint vals ghData = githubAPI (Just ghData) method endpoint vals

-- | 'githubWith', but ignoring the result.
githubWith_ :: MonadIO m => StdMethod -> Endpoint -> EndpointVals -> GitHubData -> GitHubT m ()
githubWith_ method endpoint vals ghData = void $ githubWith method endpoint vals ghData

{- API Helpers -}

-- | Set the placeholders in the given endpoint to the given values.
populateEndpoint :: EndpointVals -> Endpoint -> Text
populateEndpoint values endpoint = Text.intercalate "/" . map populate . Text.splitOn "/" $ endpoint
  where
    values' = map kvToText values
    populate t = case Text.uncons t of
      Nothing -> t
      Just (':', key) -> case lookup key values' of
        Nothing -> fail' $ "Could not find value for key '" <> key <> "'"
        Just v -> v
      Just _ -> t
    fail' msg = error . Text.unpack $ msg <> ": " <> endpoint

-- | A helper function to connect to the GitHub API.
githubAPI :: MonadIO m
  => Maybe GitHubData
  -> StdMethod
  -> Endpoint
  -> EndpointVals
  -> GitHubT m Value
githubAPI maybeData method endpoint vals = do
  GitHubConfig{..} <- ask
  let vals' = vals ++ ["owner" := ghOwner, "repo" := ghRepo]
      endpoint' = populateEndpoint vals' endpoint
      url' = Text.unpack $ "https://api.github.com" <> endpoint'
      username = Text.encodeUtf8 ghUsername
      password = Text.encodeUtf8 ghPassword
      request = applyBasicAuth username password $ parseRequest_ url'

  response <- httpLbs' ghManager request
    { method = renderStdMethod method
    , requestHeaders =
        (hAccept, "application/vnd.github.v3+json")
        : (hUserAgent, "lybot")
        : requestHeaders request
    , requestBody = RequestBodyLBS $ case maybeData of
        Nothing -> ByteString.empty
        Just d -> encode $ kvToValue d
    , checkResponse = throwErrorStatusCodes
    }

  if ByteString.null response
    then return $ object []
    else decode' response
  where
    httpLbs' manager request = liftIO $ responseBody <$> httpLbs request manager

-- | Handle the given status code, returning Left if the error was given and Right if not, and
-- the response body for each.
handleStatus :: MonadCatch m => Status -> m a -> m (Either Value a)
handleStatus status = handleJust statusException (fmap Left . decodeFromStrict) . fmap Right
  where
    decodeFromStrict = decode' . ByteString.fromStrict
    statusException (HttpExceptionRequest _ (StatusCodeException r body))
      | responseStatus r == status = Just body
    statusException _ = Nothing

{- JSON helpers -}

decode' :: (FromJSON a, Monad m) => ByteString -> m a
decode' = either fail return . eitherDecode

data KeyValue where
  (:=) :: Text -> Text -> KeyValue
  (:=*) :: (Show v, ToJSON v) => Text -> v -> KeyValue
infixr 1 :=
infixr 1 :=*

-- | Convert the given KeyValues into a JSON Object.
kvToValue :: [KeyValue] -> Value
kvToValue = object . map toPair
  where
    toPair (k := v) = (k, String v)
    toPair (k :=* v) = (k, toJSON v)

-- | Represent the given KeyValue as a pair of Texts.
kvToText :: KeyValue -> (Text, Text)
kvToText (k := v) = (k, v)
kvToText (k :=* v) = (k, v')
  where
    v' = case toJSON v of
      String t -> t
      Number scientific ->
        let rational = toRational scientific
        in if denominator rational == 1
          then showT $ numerator rational
          else showT (realToFrac rational :: Double)
      Bool b -> showT b
      _ -> error $ "Could not convert value: " ++ show v

(.:) :: FromJSON a => Value -> Text -> a
(.:) v key = either error id $ parseEither parseObject v
  where
    parseObject = withObject "parseObject" (`parseField` key)

{- Other helpers -}

showT :: Show a => a -> Text
showT = Text.pack . show
