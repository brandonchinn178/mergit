{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Data.GraphQL (Object, get)

import qualified AllTypes
import qualified Nested

allTypesResult :: Object AllTypes.Schema
allTypesResult = AllTypes.result

nestedResult :: Object Nested.Schema
nestedResult = Nested.result

main :: IO ()
main = defaultMain $ testGroup "graphql-client"
  [ testValidGetters
  , testFromObjectAllTypes
  , testFromObjectNested
  , testFromObjectNamespaced
  ]

goldens' :: String -> IO String -> TestTree
goldens' name = goldenVsString name fp . fmap ByteString.pack
  where
    fp = "test/goldens/" ++ name ++ ".golden"

goldens :: Show s => String -> s -> TestTree
goldens name = goldens' name . pure . show

testValidGetters :: TestTree
testValidGetters = testGroup "Test valid getters"
  [ goldens "bool"                     [get| allTypesResult.bool                  |]
  , goldens "lambda_bool"             ([get| .bool |] allTypesResult)
  , goldens "int"                      [get| allTypesResult.int                   |]
  , goldens "int_int2"                 [get| allTypesResult.[int,int2]            |]
  , goldens "double"                   [get| allTypesResult.double                |]
  , goldens "bool_int_double"          [get| allTypesResult.(bool,int,double)     |]
  , goldens "text"                     [get| allTypesResult.text                  |]
  , goldens "scalar"                   [get| allTypesResult.scalar                |]
  , goldens "enum"                     [get| allTypesResult.enum                  |]
  , goldens "maybeObj"                 [get| allTypesResult.maybeObject           |]
  , goldens "maybeObj_bang"            [get| allTypesResult.maybeObject!          |]
  , goldens "maybeObj_text"            [get| allTypesResult.maybeObject?.text     |]
  , goldens "maybeObj_bang_text"       [get| allTypesResult.maybeObject!.text     |]
  , goldens "maybeObjNull"             [get| allTypesResult.maybeObjectNull       |]
  , goldens "maybeObjNull_text"        [get| allTypesResult.maybeObjectNull?.text |]
  , goldens "maybeList"                [get| allTypesResult.maybeList             |]
  , goldens "maybeList_bang"           [get| allTypesResult.maybeList!            |]
  , goldens "maybeList_bang_list"      [get| allTypesResult.maybeList![]          |]
  , goldens "maybeList_bang_list_text" [get| allTypesResult.maybeList![].text     |]
  , goldens "maybeList_list"           [get| allTypesResult.maybeList?[]          |]
  , goldens "maybeList_list_text"      [get| allTypesResult.maybeList?[].text     |]
  , goldens "maybeListNull"            [get| allTypesResult.maybeListNull         |]
  , goldens "maybeListNull_list"       [get| allTypesResult.maybeListNull?[]      |]
  , goldens "maybeListNull_list_text"  [get| allTypesResult.maybeListNull?[].text |]
  , goldens "list"                     [get| allTypesResult.list                  |]
  , goldens "list_explicit"            [get| allTypesResult.list[]                |]
  , goldens "list_type"                [get| allTypesResult.list[].type           |]
  , goldens "list_maybeBool"           [get| allTypesResult.list[].maybeBool      |]
  , goldens "list_maybeInt"            [get| allTypesResult.list[].maybeInt       |]
  , goldens "nonexistent"              [get| allTypesResult.nonexistent           |]
  ]

testFromObjectAllTypes :: TestTree
testFromObjectAllTypes =
  goldens "from_object_all_types" $ map fromObj [get| allTypesResult.list |]
  where
    fromObj o = case [get| o.type |] of
      "bool" -> show [get| o.maybeBool! |]
      "int"  -> show [get| o.maybeInt!  |]
      "null" -> show [get| o.maybeNull  |]
      _ -> error "unreachable"

testFromObjectNested :: TestTree
testFromObjectNested = goldens "from_object_nested" $ map fromObj [get| nestedResult.list |]
  where
    fromObj obj = case [get| obj.a |] of
      Just field -> [get| field.b |]
      Nothing    -> [get| obj.b |]

testFromObjectNamespaced :: TestTree
testFromObjectNamespaced = goldens "from_object_namespaced" $
  map fromAllTypes [get| allTypesResult.list |]
  ++ map fromNested [get| nestedResult.list |]
  where
    fromAllTypes o = Text.unpack [get| o.type |]
    fromNested o = show [get| o.b |]
