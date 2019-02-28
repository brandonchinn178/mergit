{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example.Duration where

import Data.Aeson (FromJSON)
import Data.GraphQL
import Text.Printf (printf)

-- | Duration in milliseconds.
newtype Duration = Duration Int
  deriving (Show,FromJSON)

instance FromSchema ('SchemaCustom "Duration") where
  type SchemaResult ('SchemaCustom "Duration") = Duration

-- | Duration in (minutes, seconds).
getDuration :: Duration -> (Int, Int)
getDuration (Duration ms) = (ms `div` 1000) `divMod` 60

-- | Duration formatted to be user-friendly.
showDuration :: Duration -> String
showDuration = uncurry (printf "%d:%02dm") . getDuration
