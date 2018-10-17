{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified AllTypes

main :: IO ()
main = defaultMain $ testGroup "graphql-client"
  [ testValidGetters
  , testKeepSchema
  ]

goldens :: Show s => String -> s -> TestTree
goldens name = goldenVsString name fp . pure . ByteString.pack . show
  where
    fp = "test/goldens/" ++ name ++ ".golden"

result :: AllTypes.Result
result = AllTypes.result

testValidGetters :: TestTree
testValidGetters = testGroup "Test valid getters"
  [ goldens "bool"               [AllTypes.get| result.bool              |]
  , goldens "int"                [AllTypes.get| result.int               |]
  , goldens "double"             [AllTypes.get| result.double            |]
  , goldens "text"               [AllTypes.get| result.text              |]
  , goldens "scalar"             [AllTypes.get| result.scalar            |]
  , goldens "enum"               [AllTypes.get| result.enum              |]
  , goldens "maybeNull"          [AllTypes.get| result.maybeNull         |]
  , goldens "maybeNull_text"     [AllTypes.get| result.maybeNull.text    |]
  , goldens "maybeObj"           [AllTypes.get| result.maybeObject       |]
  , goldens "maybeObj_bang"      [AllTypes.get| result.maybeObject!      |]
  , goldens "maybeObj_text"      [AllTypes.get| result.maybeObject.text  |]
  , goldens "maybeObj_bang_text" [AllTypes.get| result.maybeObject!.text |]
  , goldens "list"               [AllTypes.get| result.list              |]
  , goldens "list_explicit"      [AllTypes.get| result.list[]            |]
  , goldens "list_type"          [AllTypes.get| result.list[].type       |]
  , goldens "list_maybeBool"     [AllTypes.get| result.list[].maybeBool  |]
  , goldens "list_maybeInt"      [AllTypes.get| result.list[].maybeInt   |]
  ]

testKeepSchema :: TestTree
testKeepSchema = goldens "list_type_contents" $ map fromObj list
  where
    list = [AllTypes.get| result.list[] > o |]
    fromObj o = case [AllTypes.get| o.type |] of
      "bool" -> show [AllTypes.get| o.maybeBool! |]
      "int"  -> show [AllTypes.get| o.maybeInt!  |]
      "null" -> show [AllTypes.get| o.maybeNull |]
      _ -> error "unreachable"
