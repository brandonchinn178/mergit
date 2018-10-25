{-|
Module      :  MergeBot.Core.GitHub.REST
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying the GitHub REST API.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module MergeBot.Core.GitHub.REST
  ( MonadGitHub(..)
  , Endpoint
  , EndpointVals
  , GitHubData
  , KeyValue(..)
  , githubAPI
  , (.:)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
    (FromJSON, ToJSON(..), Value(..), eitherDecode, encode, object, withObject)
import Data.Aeson.Types (parseEither, parseField)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as ByteStringL
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import Network.HTTP.Types

-- | A type class for monads that can query the GitHub REST API.
class MonadIO m => MonadGitHub m where
  queryGitHub :: StdMethod -> Endpoint -> EndpointVals -> GitHubData -> m Value

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
        (hAccept, "application/vnd.github.v3+json")
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
