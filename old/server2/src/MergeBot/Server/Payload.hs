{-|
Module      :  MergeBot.Server.Payload
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines a function to parse payload information from an incoming request.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MergeBot.Server.Payload
  ( GitHubPayload(..)
  , parsePayload
  ) where

import Control.Monad ((>=>))
import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Data.Aeson (eitherDecode, withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Wai (Request(..), lazyRequestBody)

import MergeBot.Server.Event (parseEventHandler)
import MergeBot.Server.Monad (Handler)

data GitHubPayload = GitHubPayload
  { ghSignature    :: Digest SHA1
  , payloadBody    :: ByteString
  , installationId :: Int
  , eventHandler   :: Handler ()
  }

-- | Parse a payload based on the given request.
parsePayload :: Request -> IO GitHubPayload
parsePayload request = do
  body <- lazyRequestBody request
  event <- getHeader request "x-github-event"
  ghSignature <- parseSignature request

  let payloadBody = ByteStringL.toStrict body
      parsePayload' = withObject "GitHubPayload" $ \o -> do
        installation <- o .: "installation"
        installationId <- installation .: "id"

        eventHandler <- parseEventHandler o event
        return GitHubPayload{..}

  either fail return $ parseEither parsePayload' =<< eitherDecode body

-- | Parse the signature from the given request.
parseSignature :: Request -> IO (Digest SHA1)
parseSignature request = do
  signature <- getHeader request "x-hub-signature"
  maybe (fail $ "Invalid signature: " ++ Text.unpack signature) return $
    case Text.splitOn "=" signature of
      ["sha1", sig] -> digestFromBase16 $ Text.encodeUtf8 sig
      _ -> Nothing
  where
    digestFromBase16 = convertFromBase16 >=> digestFromByteString

{- Helpers -}

getHeader :: Monad m => Request -> ByteString -> m Text
getHeader request name = maybe notFound (return . Text.decodeUtf8)
  $ lookup (CI.mk name) $ requestHeaders request
  where
    notFound = fail $ "Header not found: " ++ Char8.unpack name

convertFromBase16 :: ByteString -> Maybe ByteString
convertFromBase16 = either (const Nothing) Just . convertFromBase Base16
