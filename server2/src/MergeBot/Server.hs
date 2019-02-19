{-|
Module      :  MergeBot.Server
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the merge bot server running as a GitHub App.
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Server (initApp) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Lens (preview, (&), (.~), (?~))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Crypto.JWT
    ( JWK
    , JWTError
    , NumericDate(..)
    , bestJWSAlg
    , claimExp
    , claimIat
    , claimIss
    , emptyClaimsSet
    , encodeCompact
    , fromRSA
    , newJWSHeader
    , signClaims
    , stringOrUri
    )
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (Value(..), eitherDecode, parseJSON, withObject, (.:))
import Data.Aeson.Parser.Internal (decodeWith, jsonEOF)
import Data.Aeson.Types (parse)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (addUTCTime, getCurrentTime)
import Data.X509 (PrivKey(..))
import Data.X509.Memory (readKeyFileFromMemory)
import Network.Wai
    (Application, Request, Response, lazyRequestBody, requestHeaders)
import System.Environment (getEnv)
import Text.Read (readMaybe)

import MergeBot.Core.GitHub (Token(..), createToken, runSimpleREST)
import MergeBot.Core.State (BotState, newBotState)

{- Types -}

data AppEnv = AppEnv
  { ghAppId         :: Int
  , ghWebhookSecret :: Text
  , jwk             :: JWK
  }

data GitHubPayload = GitHubPayload
  { ghEvent        :: Text
  , ghSignature    :: Text -- TODO: Digest SHA1
  , payloadBody    :: ByteString
  , installationId :: Int
  , payloadData    :: Value -- TODO: better payload type
  }

parsePayload :: Request -> IO GitHubPayload
parsePayload request = do
  ghEvent <- getHeader "x-github-event"
  sig <- getHeader "x-hub-signature"
  ghSignature <- case Text.splitOn "=" sig of
    ["sha1", sig'] -> return sig'
    _ -> fail $ "Invalid signature: " ++ Text.unpack sig
  payloadBody <- lazyRequestBody request
  (installationId, payloadData) <-
    -- TODO: better debug message
    maybe (fail $ "Invalid payload: " ++ show (request, payloadBody)) return
      $ decodeWith jsonEOF (parse $ parsePayload' ghEvent) payloadBody
  return GitHubPayload{..}
  where
    getHeader name =
      maybe
        (fail $ "Header not found: " ++ Char8.unpack name)
        (return . Text.decodeUtf8)
        $ lookup (CI.mk name) $ requestHeaders request
    parsePayload' event = withObject "GitHubPayload" $ \o ->
      (,) <$> (o .: "installation" >>= (.: "id")) <*> parseJSON (Object o)

{- Monad -}

data AppState = AppState
  { ghToken :: Text
  , state   :: MVar BotState -- TODO: this should go away
  }

newtype Handler a = Handler
  { getHandler :: ReaderT AppState IO a
  }

{- App -}

initApp :: IO Application
initApp = do
  ghAppId <- getEnv "GITHUB_APP_ID" >>= parseInt
  ghWebhookSecret <- Text.pack <$> getEnv "GITHUB_WEBHOOK_SECRET"
  privateKey <- getEnv "PRIVATE_KEY_FILE" >>= loadKeyFile
  let jwk = fromRSA privateKey

  state <- newMVar newBotState -- TODO: this should go away

  return $ handleRequest AppEnv{..} state
  where
    parseInt x = maybe (fail $ "Invalid int: " ++ x) return $ readMaybe x

loadKeyFile :: FilePath -> IO PrivateKey
loadKeyFile file = do
  contents <- ByteString.readFile file
  case readKeyFileFromMemory contents of
    [PrivKeyRSA key] -> return key
    _ -> fail $ "Not a valid RSA private key file: " ++ file

handleRequest :: AppEnv -> MVar BotState -> Application
handleRequest AppEnv{..} state request respond = do
  payload <- parsePayload request
  checkSignature payload
  ghToken <- getToken payload
  respond =<< runReaderT (getHandler handleEvent) AppState{..}
  where
    checkSignature GitHubPayload{payloadBody} = undefined
    getToken GitHubPayload{installationId} = do
      alg <- either (fail . show) return =<< runExceptT @JWTError (bestJWSAlg jwk)
      now <- getCurrentTime
      let claims = emptyClaimsSet
            & claimIat ?~ NumericDate now
            & claimExp ?~ NumericDate (addUTCTime (10 * 60) now)
            & claimIss .~ preview stringOrUri (show ghAppId)
          jwsHeader = newJWSHeader ((), alg)
      jwt <- either (fail . show) return =<< runExceptT @JWTError (signClaims jwk jwsHeader claims)
      let token = BearerToken $ ByteStringL.toStrict $ encodeCompact jwt
      runSimpleREST token $ createToken installationId

handleEvent :: Handler Response
handleEvent = undefined
