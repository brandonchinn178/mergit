{-|
Module      :  Data.GraphQL.TestUtils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Defines test utilities for testing GraphQL queries.
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.GraphQL.TestUtils
  ( MockedEndpoints
  , mockWith
  , matches
  ) where

import Data.Aeson (Object, Value)
import Data.Text (Text)

import Data.GraphQL.Query.Internal (Query(..))

type MockedEndpoints = Text -> Object -> Value

-- | A helper to set for 'mockResponse' in 'QuerySettings'.
--
-- @
-- defaultQuerySettings
--   { mockResponse = mockWith
--       [ (matches Foo.query, \args -> ...)
--       , (matches Bar.query, \args -> ...)
--       ]
--   }
-- @
mockWith :: [(Text -> Bool, Object -> Value)] -> Maybe (Text -> Object -> Value)
mockWith dispatcher = Just $ \query args ->
  case filter (($ query) . fst) dispatcher of
    (_, f):_ -> f args
    _ -> error $ "No query matches: " ++ show query

-- | A helper to match 'Text' with 'Query'.
matches :: Query (api :: k) r -> Text -> Bool
matches (UnsafeQuery query) = (query ==)
