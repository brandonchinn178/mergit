{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Data.GraphQL.Test where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

import qualified Data.GraphQL.Test.AllTypes as AllTypes

data GoldenResult = forall s. Show s => GoldenResult
  { name   :: String
  , result :: s
  }

makeGoldenTest :: GoldenResult -> TestTree
makeGoldenTest GoldenResult{..} = goldenVsString name ("test/goldens/" ++ name ++ ".golden") result'
  where
    result' = pure $ ByteString.pack $ show result

test_validGetters :: IO [TestTree]
test_validGetters = do
  let result = AllTypes.result
  return $ map makeGoldenTest
    [ GoldenResult "bool"               [AllTypes.get| result.bool |]
    , GoldenResult "int"                [AllTypes.get| result.int |]
    , GoldenResult "double"             [AllTypes.get| result.double |]
    , GoldenResult "text"               [AllTypes.get| result.text |]
    , GoldenResult "scalar"             [AllTypes.get| result.scalar |]
    , GoldenResult "enum"               [AllTypes.get| result.enum |]
    , GoldenResult "maybeNull"          [AllTypes.get| result.maybeNull |]
    , GoldenResult "maybeNull_text"     [AllTypes.get| result.maybeNull.text |]
    , GoldenResult "maybeObj"           [AllTypes.get| result.maybeObject |]
    , GoldenResult "maybeObj_bang"      [AllTypes.get| result.maybeObject! |]
    , GoldenResult "maybeObj_text"      [AllTypes.get| result.maybeObject.text |]
    , GoldenResult "maybeObj_bang_text" [AllTypes.get| result.maybeObject!.text |]
    , GoldenResult "list"               [AllTypes.get| result.list |]
    , GoldenResult "list_explicit"      [AllTypes.get| result.list[] |]
    , GoldenResult "list_type"          [AllTypes.get| result.list[].type |]
    , GoldenResult "list_maybeBool"     [AllTypes.get| result.list[].maybeBool |]
    , GoldenResult "list_maybeInt"      [AllTypes.get| result.list[].maybeInt |]
    ]
