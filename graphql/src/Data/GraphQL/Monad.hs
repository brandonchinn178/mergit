{-|
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for monads that can run GraphQL queries.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Monad
  ( MonadQuery(..)
  , IsQueryable(..)
  , execQueryIO
  ) where

import Data.GraphQL.Error (GraphQLError)
import Data.GraphQL.Query (HasArgs(..), Query)

-- | A type class for monads that can run queries for the given type.
class HasArgs r => IsQueryable m r where
  execQuery :: String -> Query r -> QueryArgs r -> m (Either GraphQLError r)

-- | A type class for monads that can run queries.
--
-- A typical definition for an `IO` stack would be:
--
-- @
-- instance IsQueryable MyMonad where
--   runQuery url query args = liftIO $ execQuery "https://api.github.com/graphql" url query args
-- @
class Monad m => MonadQuery m where
  runQuery :: IsQueryable m r => Query r -> QueryArgs r -> m (Either GraphQLError r)

-- | Run the given query against the given URL and return the result.
execQueryIO :: HasArgs r => String -> Query r -> QueryArgs r -> IO (Either GraphQLError r)
execQueryIO = undefined
