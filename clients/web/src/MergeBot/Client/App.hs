{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Client.App where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Network.HTTP.Client
    (Manager, Request(..), Response(..), httpLbs, parseUrlThrow)
import Network.HTTP.Types (StdMethod, renderStdMethod)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static (Route(..), Static, base64md5)

import MergeBot.Client.Settings (AppSettings(..))
import MergeBot.Client.StaticFiles (leapyear_svg)
import MergeBot.Client.Utils (widgetFile)

data App = App
  { appSettings :: AppSettings
  , appStatic   :: Static
  , appManager  :: Manager
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
  approot = ApprootRelative

  makeSessionBackend _ = Just <$> defaultClientSessionBackend timeout fp
    where
      timeout = 120 -- minutes
      fp = "config/client_session_key.aes"

  defaultLayout contents = do
    PageContent{..} <- widgetToPageContent $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized _ _ = return Authorized

  addStaticContent ext mime content = do
    AppSettings{appStaticDir} <- appSettings <$> getYesod
    addStaticContentExternal minifym genFileName appStaticDir mkRoute ext mime content
    where
      mkRoute pieces = StaticR $ StaticRoute pieces []
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

-- | Send API requests to the merge bot server.
callAPI :: FromJSON a => StdMethod -> ByteString -> Handler a
callAPI method path = do
  App{appSettings = AppSettings{..}, appManager} <- getYesod
  baseRequest <- parseUrlThrow apiHost
  let request = baseRequest
        { port = apiPort
        , method = renderStdMethod method
        , path = path
        }
  liftIO $ either fail return . eitherDecode . responseBody =<< httpLbs request appManager
