{-|
Module      :  Servant.GitHub.Security
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions for ensuring secure communication with GitHub.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Servant.GitHub.Security
  ( loadSigner
  , parseSignature
  , doesSignatureMatch
  , getToken
  ) where

import Control.Monad ((>=>))
import Crypto.Hash (Digest, SHA1, digestFromByteString)
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.Aeson.Schema (Object, get, schema)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.REST
    ( GHEndpoint(..)
    , GitHubState(..)
    , KeyValue(..)
    , Token(..)
    , queryGitHub
    , runGitHubT
    )
import Network.HTTP.Types (StdMethod(..))
import Prelude hiding (exp)
import Web.JWT
    ( JWTClaimsSet(..)
    , Signer(..)
    , encodeSigned
    , numericDate
    , readRsaSecret
    , stringOrURI
    )

-- | Load a RSA private key as a Signer from the given file path.
loadSigner :: FilePath -> IO Signer
loadSigner file = maybe badSigner return . readSigner =<< ByteString.readFile file
  where
    badSigner = fail $ "Not a valid RSA private key file: " ++ file
    readSigner = fmap RSAPrivateKey . readRsaSecret

-- | Parse the signature from the given request.
parseSignature :: ByteString -> Maybe (Digest SHA1)
parseSignature signature =
  case Text.splitOn "=" $ Text.decodeUtf8 signature of
    ["sha1", sig] -> digestFromBase16 $ Text.encodeUtf8 sig
    _ -> Nothing
  where
    digestFromBase16 = convertFromBase16 >=> digestFromByteString

-- | Check that signing the given payload with the given key matches the given digest.
--
-- Uses `constEq` to avoid timing attacks.
doesSignatureMatch :: ByteString -> ByteString -> Digest SHA1 -> Bool
doesSignatureMatch key payload = constEq digest
  where
    digest = hmacGetDigest @SHA1 $ hmac key payload

-- | Create an installation token to use for API calls.
getToken :: Signer -> ByteString -> Int -> Int -> Int -> IO Token
getToken signer userAgent appId installationId expiry = do
  now <- getCurrentTime
  let claims = mempty
        { iat = numericDate $ utcTimeToPOSIXSeconds now
        , exp = numericDate $ utcTimeToPOSIXSeconds now + (fromIntegral expiry * 60)
        , iss = stringOrURI $ Text.pack $ show appId
        }
      jwt = encodeSigned signer claims
      token = BearerToken $ Text.encodeUtf8 jwt
  runGitHubT GitHubState{..} createToken
  where
    createToken = AccessToken . Text.encodeUtf8 . [get| .token |] <$>
      queryGitHub @_ @(Object [schema| { "token": Text } |]) GHEndpoint
        { method = POST
        , endpoint = "/app/installations/:installation_id/access_tokens"
        , endpointVals = ["installation_id" := installationId]
        , ghData = []
        }

{- Helpers -}

convertFromBase16 :: ByteString -> Maybe ByteString
convertFromBase16 = either (const Nothing) Just . convertFromBase Base16
