{-|
Module      :  MergeBot.Core.GitHub.REST
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying the GitHub REST API.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.GitHub.REST
  ( MonadGitHub(..)
  , MonadREST
  , Endpoint
  , EndpointVals
  , GitHubData
  , KeyValue(..)
  , githubTry
  , (.:)
  , kvToValue
  ) where

import Control.Monad.Catch (MonadCatch, handleJust)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
    ( FromJSON
    , ToJSON(..)
    , Value(..)
    , decode
    , eitherDecode
    , encode
    , object
    , withObject
    )
import Data.Aeson.Types (parseEither, parseField)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import Network.HTTP.Types

type MonadREST m = (MonadCatch m, MonadGitHub m)

-- | A type class for monads that can query the GitHub REST API.
class MonadIO m => MonadGitHub m where
  {-# MINIMAL getToken, getManager | queryGitHub #-}
  modifyEndpointVals :: EndpointVals -> m EndpointVals
  modifyEndpointVals = return

  getToken :: m String
  getToken = error "No token specified"

  getManager :: m Manager
  getManager = error "No manager specified"

  queryGitHub :: StdMethod -> Endpoint -> EndpointVals -> GitHubData -> m Value
  queryGitHub stdMethod endpoint endpointVals ghData = do
    token <- getToken
    manager <- getManager
    githubAPI stdMethod endpoint endpointVals ghData token manager

-- | The GitHub API endpoint with placeholders of the form ":abc" that can be replaced by
-- passed-in values.
type Endpoint = Text

-- | Mapping to populate the endpoint.
type EndpointVals = [KeyValue]

-- | Data to send to GitHub.
type GitHubData = [KeyValue]

-- | A helper function to connect to the GitHub API.
githubAPI :: MonadIO m
  => StdMethod
  -> Endpoint
  -> EndpointVals
  -> GitHubData
  -> String
  -> Manager
  -> m Value
githubAPI stdMethod endpoint vals ghData token manager = do
  response <- liftIO $ getResponse request
    { method = renderStdMethod stdMethod
    , requestHeaders =
        (hAccept, "application/vnd.github.machine-man-preview+json")
        : (hUserAgent, "LeapYear/merge-bot")
        : (hAuthorization, ByteString.pack $ "token " ++ token)
        : requestHeaders request
    , requestBody = RequestBodyLBS $ encode $ kvToValue ghData
    , checkResponse = throwErrorStatusCodes
    }

  if ByteStringL.null response
    then return $ object []
    else either fail return $ eitherDecode response
  where
    url' = Text.unpack $ "https://api.github.com" <> populateEndpoint endpoint vals
    request = parseRequest_ url'
    getResponse = fmap responseBody . flip httpLbs manager

-- | Set the placeholders in the given endpoint to the given values.
populateEndpoint :: Endpoint -> EndpointVals -> Text
populateEndpoint endpoint values = Text.intercalate "/" . map populate . Text.splitOn "/" $ endpoint
  where
    values' = map kvToText values
    populate t = case Text.uncons t of
      Nothing -> t
      Just (':', key) -> fromMaybe
        (fail' $ "Could not find value for key '" <> key <> "'")
        $ lookup key values'
      Just _ -> t
    fail' msg = error . Text.unpack $ msg <> ": " <> endpoint

{- HTTP exception handling -}

-- | Handle any exceptions thrown by the GitHub REST API.
--
-- Assuming that all client errors will be error 422, since we should always be sending valid JSON.
-- https://developer.github.com/v3/#client-errors
githubTry :: MonadCatch m => m a -> m (Either Value a)
githubTry = handleJust statusException (return . Left) . fmap Right
  where
    statusException (HttpExceptionRequest _ (StatusCodeException r body))
      | responseStatus r == status422 = decode $ ByteStringL.fromStrict body
    statusException _ = Nothing

{- Aeson helpers -}

-- | Get the given key from the Value, erroring if it doesn't exist.
(.:) :: FromJSON a => Value -> Text -> a
(.:) v key = either error id $ parseEither parseObject v
  where
    parseObject = withObject "parseObject" (`parseField` key)

{- Key/Value data -}

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

{- Other helpers -}

showT :: Show a => a -> Text
showT = Text.pack . show
