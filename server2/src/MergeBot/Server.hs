{-|
Module      :  MergeBot.Server
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the merge bot server running as a GitHub App.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MergeBot.Server (initApp) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Lens (preview, (&), (.~), (?~))
import Control.Monad (unless, (<=<))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Crypto.Hash (Digest, SHA1, digestFromByteString)
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
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson (Object, Value(..), eitherDecode, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base(..), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (addUTCTime, getCurrentTime)
import Data.X509 (PrivKey(..))
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Types (status200)
import Network.Wai
    (Application, Request, lazyRequestBody, requestHeaders, responseLBS)
import System.Environment (getEnv)
import Text.Read (readMaybe)

import MergeBot.Core.GitHub (Token(..), createToken, runSimpleREST)
import MergeBot.Core.State (BotState, newBotState)

{- Types -}

data AppEnv = AppEnv
  { ghAppId         :: Int
  , ghWebhookSecret :: ByteString
  , jwk             :: JWK
  }

{- Events -}

class GitHubEvent e where
  parseEvent :: Object -> Parser e
  runEvent :: e -> Handler ()

data GitHubPayload = GitHubPayload
  { ghSignature    :: Digest SHA1
  , installationId :: Int
  , payloadBody    :: ByteString
  , eventHandler   :: Handler ()
  }

-- | TODO: parse out values in constructors instead of just 'Value'
-- https://developer.github.com/v3/activity/events/types/#installationevent
data InstallApp = InstallApp Value

instance GitHubEvent InstallApp where
  parseEvent = return . InstallApp . Object
  runEvent _ = fail "install app"

-- | TODO: parse out values in constructors instead of just 'Value'
-- https://developer.github.com/v3/activity/events/types/#installationrepositoriesevent
data InstallRepo = InstallRepo Value

instance GitHubEvent InstallRepo where
  parseEvent = return . InstallRepo . Object
  runEvent _ = fail "install repo"

-- | Parse a payload based on the given request. Returns 'Left' if the request is for a
-- GitHub event we don't care about.
parsePayload :: Request -> IO (Either String GitHubPayload)
parsePayload request = do
  body <- lazyRequestBody request
  -- TODO: better logging
  either fail return
    $ eitherDecode body >>= parseEither (parsePayload' $ ByteStringL.toStrict body)
  where
    getHeader name =
      maybe
        (fail $ "Header not found: " ++ Char8.unpack name)
        (return . Text.decodeUtf8)
        $ lookup (CI.mk name) $ requestHeaders request
    parsePayload' payloadBody = withObject "GitHubPayload" $ \o -> do
      event <- getHeader "x-github-event"
      sig <- getHeader "x-hub-signature"
      ghSignature <- case Text.splitOn "=" sig of
        ["sha1", sig'] -> do
          decoded <- either fail return $ convertFromBase Base16 $ Text.encodeUtf8 sig'
          maybe (fail $ "Invalid SHA1 digest: " ++ Text.unpack sig') return
            $ digestFromByteString (decoded :: ByteString)
        _ -> fail $ "Invalid signature: " ++ Text.unpack sig
      installation <- o .: "installation"
      installationId <- installation .: "id"

      let mkPayload :: forall e. GitHubEvent e => Parser (Either String GitHubPayload)
          mkPayload = do
            ghEvent <- parseEvent @e o
            let eventHandler = runEvent ghEvent
            return $ Right GitHubPayload{..}
      case event of
        "installation" -> mkPayload @InstallApp
        "installation_repositories" -> mkPayload @InstallRepo
        -- TODO: other events
        _ -> return $ Left $ Text.unpack event

{- Monad -}

data AppState = AppState
  { ghToken :: Text
  , state   :: MVar BotState -- TODO: this should go away
  }

newtype Handler a = Handler
  { getHandler :: ReaderT AppState IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

{- App -}

initApp :: IO Application
initApp = do
  ghAppId <- getEnv "GITHUB_APP_ID" >>= parseInt
  ghWebhookSecret <- Char8.pack <$> getEnv "GITHUB_WEBHOOK_SECRET"
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
  parsePayload request >>= \case
    Right GitHubPayload{..} -> do
      checkSignature ghSignature payloadBody
      ghToken <- getToken installationId
      runReaderT (getHandler eventHandler) AppState{..}
    Left event -> putStrLn $ "Ignoring event: " ++ event -- TODO: better logging
  respond $ responseLBS status200 [] ""
  where
    checkSignature signature payload =
      let digest = hmacGetDigest @SHA1 $ hmac ghWebhookSecret payload
      in unless (constEq digest signature) $ fail "Signature does not match payload"
    getToken installId = runJWT $ do
      alg <- bestJWSAlg jwk
      now <- liftIO getCurrentTime
      let claims = emptyClaimsSet
            & claimIat ?~ NumericDate now
            & claimExp ?~ NumericDate (addUTCTime (10 * 60) now)
            & claimIss .~ preview stringOrUri (show ghAppId)
          jwsHeader = newJWSHeader ((), alg)
      jwt <- signClaims jwk jwsHeader claims
      let token = BearerToken $ ByteStringL.toStrict $ encodeCompact jwt
      liftIO $ runSimpleREST token $ createToken installId
    runJWT = either (fail . show) return <=< runExceptT @JWTError
