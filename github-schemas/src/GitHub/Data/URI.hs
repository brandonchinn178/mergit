{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  GitHub.Data.URI
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the URI data type.
-}
module GitHub.Data.URI where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text

newtype URI = URI {unURI :: Text}
  deriving (Show, Eq, FromJSON, ToJSON)

unURI' :: URI -> String
unURI' = Text.unpack . unURI
