{-# LANGUAGE RecordWildCards #-}

module MergeBot.Client
  ( appMain
  , initApp
  ) where

import Yesod
import Yesod.Static (static)

import MergeBot.Client.App (App(..))
import MergeBot.Client.CommandLine (AppOptions(..), parseOptions)
import MergeBot.Client.Handlers ()
import MergeBot.Client.Settings (AppSettings(..), appInit, loadAppSettings)

initApp :: AppSettings -> IO App
initApp appSettings = do
  appStatic <- static $ appStaticDir appSettings
  return App{..}

appMain :: IO ()
appMain = do
  AppOptions{..} <- parseOptions
  appSettings <- loadAppSettings configFile
  appInit
  warp (appPort appSettings) =<< initApp appSettings
