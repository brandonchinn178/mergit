{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  GitHub.Data.GitObjectID
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GitObjectID data type.
-}
module GitHub.Data.GitObjectID where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import Text.Printf (PrintfArg)

newtype GitObjectID = GitObjectID {unOID :: Text}
  deriving (Show, Eq, FromJSON, ToJSON, IsString, PrintfArg)
