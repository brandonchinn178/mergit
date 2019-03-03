{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Example.GraphQL.Enums.ReleaseStatus where

import Data.Aeson (FromJSON(..), withText)
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
