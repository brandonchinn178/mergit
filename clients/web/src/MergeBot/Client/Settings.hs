{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MergeBot.Client.Settings
  ( AppSettings(..)
  , loadAppSettings
  , appReloadTemplates
  , appInit
  , compileTimeStaticDir
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON(..), withObject, (.:))
import qualified Data.Text as Text
import Data.Yaml (decodeFileThrow)
import Language.Haskell.TH (Loc(..), runIO)
import Language.Haskell.TH.Syntax (lift, qLocation)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))

import MergeBot.Client.Settings.Development (isDevelopment)

data AppSettings = AppSettings
  { appPort      :: Int
  , appStaticDir :: FilePath
  , apiHost      :: String
  , apiPort      :: Int
  } deriving (Show)

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o ->
    AppSettings
      <$> o .: "port"
      <*> o .: "static-dir"
      <*> o .: "api-host"
      <*> o .: "api-port"

-- | Load settings from the given configuration file.
loadAppSettings :: FilePath -> IO AppSettings
loadAppSettings = decodeFileThrow

-- | True if static file templates should be reloaded while running the web server.
appReloadTemplates :: Bool
appReloadTemplates = isDevelopment

-- | Any action to run when starting the web server.
appInit :: IO ()
appInit = when isDevelopment $ setCurrentDirectory topDir

-- | Static directory with static files at compile time.
compileTimeStaticDir :: FilePath
compileTimeStaticDir = topDir </> "static/"

{- Helpers -}

-- | The root of this Haskell project; i.e. the location of 'package.yaml' for this project.
topDir :: FilePath
topDir = $(do
  path <- loc_filename <$> qLocation
  let dropEnd i xs = take (length xs - i) xs
  case Text.unpack . Text.intercalate "/" . dropEnd 4 . Text.splitOn "/" . Text.pack $ path of
    -- stack build
    "" -> lift =<< runIO getCurrentDirectory
    -- stack ghci
    path' -> lift path'
  )
