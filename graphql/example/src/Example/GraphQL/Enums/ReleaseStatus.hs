{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example.GraphQL.Enums.ReleaseStatus where

import Data.Aeson (FromJSON(..), withText)
import Data.GraphQL
import qualified Data.Text as Text

data ReleaseStatus
  = OFFICIAL
  | PROMOTION
  | BOOTLEG
  | PSEUDORELEASE
  deriving (Show,Eq,Enum)

instance FromJSON ReleaseStatus where
  parseJSON = withText "ReleaseStatus" $ \case
    "OFFICIAL" -> pure OFFICIAL
    "PROMOTION" -> pure PROMOTION
    "BOOTLEG" -> pure BOOTLEG
    "PSEUDORELEASE" -> pure PSEUDORELEASE
    t -> fail $ "Bad ReleaseStatus: " ++ Text.unpack t

instance FromSchema ('SchemaCustom "ReleaseStatus") where
  type SchemaResult ('SchemaCustom "ReleaseStatus") = ReleaseStatus
