{-|
Module      :  Servant.GitHub.Security
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions for ensuring secure communication with GitHub.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Servant.GitHub.Security
  ( loadSigner
  , parseSignature
  , doesSignatureMatch
  , getJWTToken
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
import Data.Time (addUTCTime, getCurrentTime)
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

-- | Create a JWT token that expires in 10 minutes.
getJWTToken :: Signer -> Int -> IO Token
getJWTToken signer appId = mkToken <$> getNow
  where
    mkToken now =
      let claims = mempty
            { iat = numericDate $ utcTimeToPOSIXSeconds now
            , exp = numericDate $ utcTimeToPOSIXSeconds now + (10 * 60)
            , iss = stringOrURI $ Text.pack $ show appId
            }
          jwt = encodeSigned signer claims
      in BearerToken $ Text.encodeUtf8 jwt
    -- lose a second in the case of rounding
    -- https://github.community/t5/GitHub-API-Development-and/quot-Expiration-time-claim-exp-is-too-far-in-the-future-quot/m-p/20457/highlight/true#M1127
    getNow = addUTCTime (-1) <$> getCurrentTime

-- | Create an installation token.
getToken :: Signer -> Int -> ByteString -> Int -> IO Token
getToken signer appId userAgent installationId = do
  jwtToken <- getJWTToken signer appId
  let state = GitHubState
        { token = jwtToken
        , apiVersion = "machine-man-preview"
        , userAgent
        }
  runGitHubT state $ AccessToken . Text.encodeUtf8 . [get| .token |] <$> createToken
  where
    createToken =
      queryGitHub @_ @(Object [schema| { token: Text } |]) GHEndpoint
        { method = POST
        , endpoint = "/app/installations/:installation_id/access_tokens"
        , endpointVals = ["installation_id" := installationId]
        , ghData = []
        }

{- Helpers -}

convertFromBase16 :: ByteString -> Maybe ByteString
convertFromBase16 = either (const Nothing) Just . convertFromBase Base16
