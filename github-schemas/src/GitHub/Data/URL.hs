{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  GitHub.Data.URL
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the URL data type.
-}
module GitHub.Data.URL where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

newtype URL = URL {unURL :: Text}
  deriving (Show, FromJSON, ToJSON)
