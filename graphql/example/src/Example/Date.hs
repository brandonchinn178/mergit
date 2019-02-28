{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Example.Date where

import Data.Aeson (FromJSON(..), withText)
import Data.GraphQL
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import qualified Data.Text as Text

data Date = Date
  { year  :: Int
  , month :: Maybe Int
  , day   :: Maybe Int
  }
  deriving (Show)

instance FromJSON Date where
  parseJSON = withText "Date" $ \s ->
    case map (read . Text.unpack) $ Text.splitOn "-" s of
      [y] -> return $ Date y Nothing Nothing
      [y,m] -> return $ Date y (Just m) Nothing
      [y,m,d] -> return $ Date y (Just m) (Just d)
      v -> fail $ "Invalid Date: " ++ show v

instance FromSchema ('SchemaCustom "Date") where
  type SchemaResult ('SchemaCustom "Date") = Date

showDate :: Date -> String
showDate Date{..} = intercalate "-" $ map show $ [year] ++ maybeToList month ++ maybeToList day
