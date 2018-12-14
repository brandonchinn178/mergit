{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Date where

import Data.GraphQL
import Data.GraphQL.Aeson (Value(..))
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import qualified Data.Text as Text

data Date = Date
  { year  :: Int
  , month :: Maybe Int
  , day   :: Maybe Int
  }
  deriving (Show)

instance GraphQLScalar Date where
  getScalar = \case
    String s -> case map (read . Text.unpack) $ Text.splitOn "-" s of
      [y] -> Date y Nothing Nothing
      [y,m] -> Date y (Just m) Nothing
      [y,m,d] -> Date y (Just m) (Just d)
      v -> error $ "Invalid Date: " ++ show v
    v -> error $ "Invalid Date: " ++ show v

type instance ToScalar "Date" = Date

instance FromSchema Date where
  type ToSchema Date = 'SchemaScalar "Date"
  parseValue = parseValueScalar

showDate :: Date -> String
showDate Date{..} = intercalate "-" $ map show $ [year] ++ maybeToList month ++ maybeToList day
