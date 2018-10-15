{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.GraphQL.Query
  ( Query
  , HasArgs(..)
  , readGraphQLFile
  , object
  -- * Re-exports
  , (.=)
  ) where

import Data.Aeson (Object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Language.Haskell.TH (ExpQ, runIO)
import Language.Haskell.TH.Syntax (lift)
import Path (fromAbsFile, parseRelFile, reldir, (</>))
import Path.IO (getCurrentDir)

import Data.GraphQL.Query.Internal

-- | An alias for HashMap.fromList.
object :: [Pair] -> Object
object = HashMap.fromList

-- | A type class for GraphQL queries with arguments.
class HasArgs r where
  type QueryArgs r = a | a -> r
  fromArgs :: QueryArgs r -> Object

-- | A temporary function to read a graphql file and output it as a Query.
--
-- This function should go away when we generate the entire file with Template Haskell.
readGraphQLFile :: FilePath -> ExpQ
readGraphQLFile fp = do
  query <- runIO $ do
    cwd <- getCurrentDir
    file <- parseRelFile fp
    readFile (fromAbsFile $ cwd </> [reldir|graphql|] </> file)
  [| Query $ Text.pack $(lift query) |]
