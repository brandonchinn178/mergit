{-# LANGUAGE RecordWildCards #-}

module MergeBot.Client
  ( appMain
  , initApp
  ) where

import Yesod
import Yesod.Static (static)

import MergeBot.Client.App (App(..))
import MergeBot.Client.Handlers ()
import MergeBot.Client.Settings (AppSettings(..), appSettings)

initApp :: IO App
initApp = do
  appStatic <- static $ appStaticDir appSettings
  return App{..}

appMain :: IO ()
appMain = do
  appInit appSettings
  warp (appPort appSettings) =<< initApp
