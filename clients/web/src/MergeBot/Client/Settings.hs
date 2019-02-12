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
  , appReloadTemplates = appDevel
  , appInit = when appDevel $ setCurrentDirectory topDir
  }
  where
    appDevel =
      #ifdef DEVELOPMENT
        True
      #else
        False
      #endif
    topDir = $(runIO getCurrentDirectory >>= lift)
