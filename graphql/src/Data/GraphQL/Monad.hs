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
{-# LANGUAGE OverloadedStrings #-}
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
  -- * Re-exports
  , MonadIO
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Network.HTTP.Client
    ( Manager
    , ManagerSettings
    , Request(..)
    , RequestBody(..)
    , httpLbs
    , newManager
    , parseUrlThrow
    , responseBody
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentType)

import Data.GraphQL.Query (HasArgs(..))
import Data.GraphQL.Query.Internal (Query(..))
import Data.GraphQL.Result (GraphQLResult(..))

-- | A type class for queryable results.
class HasArgs r => IsQueryable r where
  execQuery :: MonadIO m => Query r -> QueryArgs r -> QueryT m (GraphQLResult r)

-- | A type class for monads that can run queries.
class MonadIO m => MonadQuery m where
  runQuery :: IsQueryable r => Query r -> QueryArgs r -> m (GraphQLResult r)

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
newtype QueryT m a = QueryT { unQueryT :: ReaderT QueryState m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader QueryState
    )

instance MonadIO m => MonadQuery (QueryT m) where
  runQuery = execQuery

-- | The settings for running QueryT.
data QuerySettings = QuerySettings
  { managerSettings :: ManagerSettings
    -- ^ Uses TLS by default
  , url             :: String
  , modifyReq       :: Request -> Request
  }

-- | Default query settings.
defaultQuerySettings :: QuerySettings
defaultQuerySettings = QuerySettings
  { managerSettings = tlsManagerSettings
  , url = error "No URL is provided"
  , modifyReq = id
  }

-- | The state for running QueryT.
data QueryState = QueryState
  { manager :: Manager
  , baseReq :: Request
  }

-- | Run a QueryT stack.
runQueryT :: (MonadThrow m, MonadIO m) => QuerySettings -> QueryT m a -> m a
runQueryT QuerySettings{..} query = do
  manager <- liftIO $ newManager managerSettings
  baseReq <- modifyReq . modifyReq' <$> parseUrlThrow url
  (`runReaderT` QueryState{..})
    . unQueryT
    $ query
  where
    modifyReq' req = req
      { method = "POST"
      , requestHeaders = (hContentType, "application/json") : requestHeaders req
      }

-- | Run the given query within a QueryT.
execQueryFor :: (MonadIO m, HasArgs r)
  => (Value -> r) -> Query r -> QueryArgs r -> QueryT m (GraphQLResult r)
execQueryFor fromValue (Query query) args = do
  QueryState{..} <- ask
  let requestObject = object
        [ "query" .= query
        , "variables" .= fromArgs args
        ]
      request = baseReq { requestBody = RequestBodyLBS $ encode requestObject }
  response <- liftIO $ httpLbs request manager
  either fail (return . fmap fromValue) $ eitherDecode $ responseBody response
