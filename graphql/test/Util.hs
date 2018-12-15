{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Util where

import Data.Aeson (Value(..), decodeFileStrict)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

import Data.GraphQL.Schema.Internal (Object(..))

getMockedResult :: FilePath -> ExpQ
getMockedResult fp = runIO (decodeFileStrict fp) >>= \case
  Just (Object o) ->
    let o' = map (first Text.unpack) $ HashMap.toList o
    in [| asObject $(lift o') |]
  result -> error $ "Bad result: " ++ show result

asObject :: [(String, Value)] -> Object schema
asObject = UnsafeObject . HashMap.fromList . map (first Text.pack)
