{-|
Module      :  MergeBot.Core.GraphQL.Scalars
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines scalars defined in the GraphQL schema.
-}

module MergeBot.Core.GraphQL.Scalars where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.ISO8601 (parseISO8601)

-- | Parse an ISO8601-formatted UTCTime string.
parseUTCTime :: Text -> UTCTime
parseUTCTime time = fromMaybe (error $ "Invalid UTCTime: " ++ time') $ parseISO8601 time'
  where
    time' = Text.unpack time
