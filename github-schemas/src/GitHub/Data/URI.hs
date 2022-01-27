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
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Printf (PrintfArg)

newtype URI = URI {unURI :: Text}
  deriving (Show, Eq, FromJSON, ToJSON, IsString, PrintfArg)

unURI' :: URI -> String
unURI' = Text.unpack . unURI
