{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphQL.Query
  ( Query
  , HasArgs(..)
  , readGraphQLFile
  , object
  , module Query
  -- * Re-exports
  , Object
  , (.=)
  ) where

import Data.Aeson (Object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy)
import Language.Haskell.TH

import Data.GraphQL.Query.Internal
import Data.GraphQL.Query.Schema as Query

-- | An alias for HashMap.fromList.
object :: [Pair] -> Object
object = HashMap.fromList

-- | A type class for GraphQL queries with arguments.
class HasArgs r where
  type QueryArgs r
  fromArgs :: Proxy r -> QueryArgs r -> Object

-- | A temporary function to read a graphql file and output it as a Query.
--
-- This function should go away when we generate the entire file with Template Haskell.
readGraphQLFile :: FilePath -> ExpQ
readGraphQLFile = undefined
