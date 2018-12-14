{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Duration where

import Data.GraphQL
import Data.GraphQL.Aeson (Value(..), toInt)
import Text.Printf (printf)

-- | Duration in milliseconds.
newtype Duration = Duration Int
  deriving (Show)

instance GraphQLScalar Duration where
  getScalar = \case
    Number n | Just i <- toInt n -> Duration i
    v -> error $ "Invalid Duration: " ++ show v

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
