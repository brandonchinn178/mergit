{-|
Module      :  MergeBot.Core.GraphQL.Scalars.HTML
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the HTML scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.HTML where

import Data.GraphQL
import Data.GraphQL.Aeson (FromJSON)
import Data.Text (Text)

newtype HTML = HTML { unHTML :: Text }
  deriving (Show,FromJSON)

instance GraphQLScalar HTML

type instance ToScalar "HTML" = HTML

instance FromSchema HTML where
  type ToSchema HTML = 'SchemaScalar "HTML"
  parseValue = parseValueScalar
