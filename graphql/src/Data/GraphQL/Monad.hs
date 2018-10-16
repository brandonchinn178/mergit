{-|
Module      :  Data.GraphQL.Monad
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for monads that can run GraphQL queries.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Monad
  ( IsQueryable(..)
  , MonadQuery(..)
  , QueryT(..)
  , QuerySettings(..)
  , defaultQuerySettings
  , runQueryT
  , execQueryFor
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Aeson (Value)
import Network.HTTP.Client
    (Manager, ManagerSettings, Request, defaultManagerSettings, newManager)

import Data.GraphQL.Error (GraphQLError)
import Data.GraphQL.Query (HasArgs(..), Query)

-- | A type class for monads that can run queries for the given type.
--
-- Instances for this type class are typically generated for GraphQL results, for the 'QueryT m'
-- monad.
class HasArgs r => IsQueryable m r where
  execQuery :: Query r -> QueryArgs r -> m (Either GraphQLError r)

-- | A type class for monads that can run queries.
class Monad m => MonadQuery m where
  runQuery :: IsQueryable m r => Query r -> QueryArgs r -> m (Either GraphQLError r)

-- | The monad transformer type that should be used to run GraphQL queries.
--
-- @
-- newtype MyMonad a = MyMonad { unMyMonad :: QueryT IO a }
--
-- runMyMonad :: MyMonad a -> IO a
-- runMyMonad = runQueryT querySettings . unMyMonad
--   where
--     querySettings = defaultQuerySettings
--       { url = "https://api.github.com/graphql"
--       , modifyReq = \req -> req
--           { requestHeaders =
--               (hAuthorization, "bearer my_github_token") : requestHeaders req
--           }
--       }
-- @
newtype QueryT m a = QueryT { unQueryT :: ReaderT QuerySettings m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader QuerySettings
    )

instance Monad m => MonadQuery (QueryT m) where
  runQuery :: IsQueryable (QueryT m) r => Query r -> QueryArgs r -> QueryT m (Either GraphQLError r)
  runQuery = execQuery

-- | The settings for running QueryT.
data QuerySettings = QuerySettings
  { manager         :: Maybe Manager
    -- ^ if none is provided, a new manager will be allocated with runQueryT
  , managerSettings :: ManagerSettings
    -- ^ used if 'manager' is Nothing
  , url             :: String
  , modifyReq       :: Request -> Request
  }

-- | Default query settings.
defaultQuerySettings :: QuerySettings
defaultQuerySettings = QuerySettings
  { manager = Nothing
  , managerSettings = defaultManagerSettings
  , url = error "No URL is provided"
  , modifyReq = id
  }

-- | Run a QueryT stack.
runQueryT :: MonadIO m => QuerySettings -> QueryT m a -> m a
runQueryT settings@QuerySettings{..} query = do
  manager' <- liftIO $ maybe (newManager managerSettings) return manager
  let settings' = settings { manager = Just manager' }
  (`runReaderT` settings')
    . unQueryT
    $ query

-- | Run the given query within a QueryT.
execQueryFor :: HasArgs r
  => (Value -> r) -> Query r -> QueryArgs r -> QueryT m (Either GraphQLError r)
execQueryFor = undefined
