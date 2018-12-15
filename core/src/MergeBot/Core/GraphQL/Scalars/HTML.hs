{-|
Module      :  MergeBot.Core.GraphQL.Scalars.HTML
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the HTML scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.HTML where

import Data.GraphQL
import Data.GraphQL.Aeson (Value(..))
import Data.Text (Text)

newtype HTML = HTML { unHTML :: Text }
  deriving (Show)

instance GraphQLScalar HTML where
  getScalar = \case
    String s -> HTML s
    v -> error $ "Invalid HTML: " ++ show v

type instance ToScalar "HTML" = HTML

instance FromSchema HTML where
  type ToSchema HTML = 'SchemaScalar "HTML"
  parseValue = parseValueScalar
