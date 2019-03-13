{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Example.GraphQL.Custom.ReleaseStatus where

import Data.Aeson.Schema.TH (genFromJSONEnum)

data ReleaseStatus
  = OFFICIAL
  | PROMOTION
  | BOOTLEG
  | PSEUDORELEASE
  deriving (Show,Eq,Enum)

genFromJSONEnum ''ReleaseStatus
