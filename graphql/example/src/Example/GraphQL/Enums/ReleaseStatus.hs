{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Example.GraphQL.Enums.ReleaseStatus where

import Data.GraphQL
import qualified Data.Text as Text

data ReleaseStatus
  = OFFICIAL
  | PROMOTION
  | BOOTLEG
  | PSEUDORELEASE
  deriving (Show,Eq,Enum)

instance GraphQLEnum ReleaseStatus where
  getEnum s = case Text.unpack s of
    "OFFICIAL" -> OFFICIAL
    "PROMOTION" -> PROMOTION
    "BOOTLEG" -> BOOTLEG
    "PSEUDORELEASE" -> PSEUDORELEASE
    _ -> error $ "Bad ReleaseStatus: " ++ Text.unpack s

type instance ToEnum "ReleaseStatus" = ReleaseStatus

instance FromSchema ReleaseStatus where
  type ToSchema ReleaseStatus = 'SchemaEnum "ReleaseStatus"
  parseValue = parseValueEnum
