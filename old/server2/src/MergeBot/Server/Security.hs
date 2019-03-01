{-|
Module      :  MergeBot.Server.Security
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines functions useful for verifying incoming requests.
-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Server.Security
  ( loadSigner
  , checkSignature
  , getToken
  ) where

import Control.Monad (unless)
import Crypto.Hash (Digest, SHA1)
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.ByteArray (constEq)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Prelude hiding (exp)
import Web.JWT
    ( JWTClaimsSet(..)
    , Signer(..)
    , encodeSigned
    , numericDate
    , readRsaSecret
    , stringOrURI
    )

import MergeBot.Core.GitHub (Token(..), createToken, runSimpleREST)

-- | Load a RSA private key as a Signer from the given file path.
loadSigner :: FilePath -> IO Signer
loadSigner file = maybe badSigner return . readSigner =<< ByteString.readFile file
  where
    badSigner = fail $ "Not a valid RSA private key file: " ++ file
    readSigner = fmap RSAPrivateKey . readRsaSecret

-- | Check that signing the given payload with the given key matches the given digest.
--
-- Uses `constEq` to avoid timing attacks.
checkSignature :: Monad m => ByteString -> ByteString -> Digest SHA1 -> m ()
checkSignature key payload digest =
  let newDigest = hmacGetDigest @SHA1 $ hmac key payload
  in unless (constEq digest newDigest) $ fail "Signature does not match payload"

-- | Create an installation token to use for API calls.
getToken :: Signer -> Int -> Int -> IO Text
getToken signer appId installationId = do
  now <- getCurrentTime
  let claims = mempty
        { iat = numericDate $ utcTimeToPOSIXSeconds now
        , exp = numericDate $ utcTimeToPOSIXSeconds now + (10 * 60)
        , iss = stringOrURI $ Text.pack $ show appId
        }
      jwt = encodeSigned signer claims
      token = BearerToken $ Text.encodeUtf8 jwt
  runSimpleREST token $ createToken installationId
