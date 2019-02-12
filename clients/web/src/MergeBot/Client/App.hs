{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Client.App where

import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static (Route(..), Static, base64md5)

import MergeBot.Client.Settings (AppSettings(..), appSettings)
import MergeBot.Client.StaticFiles (leapyear_svg)
import MergeBot.Client.Utils (widgetFile)

data App = App
  { appStatic   :: Static
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

  addStaticContent ext mime content =
    addStaticContentExternal minifym genFileName staticDir mkRoute ext mime content
    where
      staticDir = appStaticDir appSettings
      mkRoute pieces = StaticR $ StaticRoute pieces []
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs
