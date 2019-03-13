{-|
Module      :  MergeBot.Core.GraphQL.Custom.GitObjectID
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GitObjectID data type.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MergeBot.Core.GraphQL.Custom.GitObjectID where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)

newtype GitObjectID = GitObjectID { unOID :: Text }
  deriving (Show,FromJSON,ToJSON)
