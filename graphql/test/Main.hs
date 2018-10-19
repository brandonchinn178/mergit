{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified AllTypes
import qualified Nested

main :: IO ()
main = defaultMain $ testGroup "graphql-client"
  [ testValidGetters
  , testKeepSchema
  , testKeepSchemaNested
  ]

goldens :: Show s => String -> s -> TestTree
goldens name = goldenVsString name fp . pure . ByteString.pack . show
  where
    fp = "test/goldens/" ++ name ++ ".golden"

testValidGetters :: TestTree
testValidGetters = testGroup "Test valid getters"
  [ goldens "bool"               [AllTypes.get| result.bool                 |]
  , goldens "int"                [AllTypes.get| result.int                  |]
  , goldens "double"             [AllTypes.get| result.double               |]
  , goldens "text"               [AllTypes.get| result.text                 |]
  , goldens "scalar"             [AllTypes.get| result.scalar               |]
  , goldens "enum"               [AllTypes.get| result.enum                 |]
  , goldens "maybeObj"           [AllTypes.get| result.maybeObject          |]
  , goldens "maybeObj_bang"      [AllTypes.get| result.maybeObject!         |]
  , goldens "maybeObj_text"      [AllTypes.get| result.maybeObject.text     |]
  , goldens "maybeObj_bang_text" [AllTypes.get| result.maybeObject!.text    |]
  , goldens "maybeObjNull"       [AllTypes.get| result.maybeObjectNull      |]
  , goldens "maybeObjNull_text"  [AllTypes.get| result.maybeObjectNull.text |]
  , goldens "maybeList"                [AllTypes.get| result.maybeList            |]
  , goldens "maybeList_bang"           [AllTypes.get| result.maybeList!           |]
  , goldens "maybeList_bang_list"      [AllTypes.get| result.maybeList![]         |]
  , goldens "maybeList_bang_list_text" [AllTypes.get| result.maybeList![].text    |]
  -- , goldens "maybeList_list"           [AllTypes.get| result.maybeList[]          |]
  -- , goldens "maybeList_list_text"      [AllTypes.get| result.maybeList[].text     |]
  , goldens "maybeListNull"            [AllTypes.get| result.maybeListNull        |]
  -- , goldens "maybeListNull_list"       [AllTypes.get| result.maybeListNull[]      |]
  -- , goldens "maybeListNull_list_text"  [AllTypes.get| result.maybeListNull[].text |]
  , goldens "list"               [AllTypes.get| result.list                 |]
  , goldens "list_explicit"      [AllTypes.get| result.list[]               |]
  , goldens "list_type"          [AllTypes.get| result.list[].type          |]
  , goldens "list_maybeBool"     [AllTypes.get| result.list[].maybeBool     |]
  , goldens "list_maybeInt"      [AllTypes.get| result.list[].maybeInt      |]
  , goldens "nonexistent"        [AllTypes.get| result.nonexistent          |]
  ]
  where
    result = AllTypes.result

testKeepSchema :: TestTree
testKeepSchema = goldens "list_type_contents" $ map fromObj list
  where
    result = AllTypes.result
    list = [AllTypes.get| result.list[] > o |]
    fromObj o = case [AllTypes.get| @o.type |] of
      "bool" -> show [AllTypes.get| @o.maybeBool! |]
      "int"  -> show [AllTypes.get| @o.maybeInt!  |]
      "null" -> show [AllTypes.get| @o.maybeNull  |]
      _ -> error "unreachable"

testKeepSchemaNested :: TestTree
testKeepSchemaNested = goldens "keep_schema_nested" $ map fromObj list
  where
    result = Nested.result
    list = [Nested.get| result.list[] > o |]
    fromObj o = case [Nested.get| @o.a > field |] of
      Just field -> [Nested.get| @field.b |]
      Nothing    -> [Nested.get| @o.b     |]
