{-|
Module      :  GitHub.Data.GitObjectID
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GitObjectID data type.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GitHub.Data.GitObjectID where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text

newtype GitObjectID = GitObjectID { unOID :: Text }
  deriving (Show,FromJSON,ToJSON)

unOID' :: GitObjectID -> String
unOID' = Text.unpack . unOID
