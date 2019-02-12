{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Client.Settings
  ( AppSettings(..)
  , appSettings
  ) where

import Control.Monad (when)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

import MergeBot.Client.Settings.Development (isDevelopment)

data AppSettings = AppSettings
  { appPort            :: Int
  , appStaticDir       :: FilePath
  , appReloadTemplates :: Bool
  , appInit            :: IO ()
  }

appSettings :: AppSettings
appSettings = AppSettings
  { appPort = 8080
  , appStaticDir = "static/"
  , appReloadTemplates = isDevelopment
  , appInit = when isDevelopment $ setCurrentDirectory topDir
  }
  where
    topDir = $(runIO getCurrentDirectory >>= lift)
