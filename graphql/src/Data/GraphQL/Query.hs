{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}

module Data.GraphQL.Query
  ( Query
  , GraphQLArgs(..)
  , fromQuery
  , queryName
  , readGraphQLFile
  ) where

import Data.Aeson (Object)
import Data.Aeson.Schema (SchemaType)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH (ExpQ, Loc(..), location, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import Path
    ( filename
    , fromAbsFile
    , fromRelFile
    , parent
    , parseAbsFile
    , parseRelFile
    , (</>)
    )
import Path.IO (getCurrentDir)


-- | A type class for query arguments.
class GraphQLArgs args where
  fromArgs :: args -> Object

-- | A GraphQL Query that is validated at compile-time.
data Query (api :: k) (args :: Type) (schema :: SchemaType) = UnsafeQuery
  { queryName' :: String
  , queryText  :: Text
  }
  deriving (Show)

-- | Extract the text of the Query.
fromQuery :: Query api args r -> Text
fromQuery = queryText

-- | Get the name of the Query.
queryName :: Query api args r -> String
queryName = queryName'

-- | A temporary function to read a graphql file and output it as a Query.
--
-- This function should go away when we generate the entire file with Template Haskell.
readGraphQLFile :: FilePath -> ExpQ
readGraphQLFile fp = do
  loc <- loc_filename <$> location
  here <- case loc of
    '/':_ -> parseAbsFile loc
    _ -> do
      cwd <- runIO getCurrentDir
      loc' <- parseRelFile loc
      return $ cwd </> loc'
  file <- parseRelFile fp
  query <- readFile' $ fromAbsFile $ parent here </> file
  let name = fromRelFile $ filename file
  [| UnsafeQuery name $ Text.pack $(lift query) |]
  where
    readFile' file = do
      addDependentFile file
      runIO $ readFile file
