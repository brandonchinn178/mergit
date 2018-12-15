{-|
Module      :  Data.GraphQL.Query
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions needed by GraphQL queries.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.GraphQL.Query
  ( Query
  , readGraphQLFile
  ) where

import qualified Data.Text as Text
import Language.Haskell.TH (ExpQ, Loc(..), location, runIO)
import Language.Haskell.TH.Syntax (lift)
import Path (fromAbsFile, parent, parseAbsFile, parseRelFile, (</>))
import Path.IO (getCurrentDir)

import Data.GraphQL.Query.Internal

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
  query <- runIO $ do
    file <- parseRelFile fp
    readFile (fromAbsFile $ parent here </> file)
  [| UnsafeQuery $ Text.pack $(lift query) |]
