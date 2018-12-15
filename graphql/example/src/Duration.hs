{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Duration where

import Data.Aeson (FromJSON)
import Data.GraphQL
import Text.Printf (printf)

-- | Duration in milliseconds.
newtype Duration = Duration Int
  deriving (Show,FromJSON)

instance GraphQLScalar Duration

type instance ToScalar "Duration" = Duration

instance FromSchema Duration where
  type ToSchema Duration = 'SchemaScalar "Duration"
  parseValue = parseValueScalar

-- | Duration in (minutes, seconds).
getDuration :: Duration -> (Int, Int)
getDuration (Duration ms) = (ms `div` 1000) `divMod` 60

-- | Duration formatted to be user-friendly.
showDuration :: Duration -> String
showDuration = uncurry (printf "%d:%02dm") . getDuration
