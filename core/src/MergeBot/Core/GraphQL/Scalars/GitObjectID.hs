{-|
Module      :  MergeBot.Core.GraphQL.Scalars.GitObjectID
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines the GitObjectID scalar
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MergeBot.Core.GraphQL.Scalars.GitObjectID where

import Data.GraphQL
import Data.GraphQL.Aeson (FromJSON)
import Data.Text (Text)

newtype GitObjectID = GitObjectID { unOID :: Text }
  deriving (Show,FromJSON)

instance GraphQLScalar GitObjectID

type instance ToScalar "GitObjectID" = GitObjectID

instance FromSchema GitObjectID where
  type ToSchema GitObjectID = 'SchemaScalar "GitObjectID"
  parseValue = parseValueScalar
