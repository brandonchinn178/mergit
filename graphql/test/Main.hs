{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import Language.Haskell.TH (runIO, runQ)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Data.GraphQL (Schema(..))
import Data.GraphQL.Result.Getter.Internal (generateGetter')

import qualified AllTypes
import qualified Nested

main :: IO ()
main = defaultMain $ testGroup "graphql-client"
  [ testValidGetters
  , testKeepSchemaAllTypes
  , testKeepSchemaNested
  , testKeepSchemaNamespaced
  , testInvalidGetters
  ]

goldens' :: Show s => String -> IO s -> TestTree
goldens' name = goldenVsString name fp . fmap (ByteString.pack . show)
  where
    fp = "test/goldens/" ++ name ++ ".golden"

goldens :: Show s => String -> s -> TestTree
goldens name = goldens' name . pure

testValidGetters :: TestTree
testValidGetters = testGroup "Test valid getters"
  [ goldens "bool"                     [AllTypes.get| result.bool                 |]
  , goldens "int"                      [AllTypes.get| result.int                  |]
  , goldens "double"                   [AllTypes.get| result.double               |]
  , goldens "text"                     [AllTypes.get| result.text                 |]
  , goldens "scalar"                   [AllTypes.get| result.scalar               |]
  , goldens "enum"                     [AllTypes.get| result.enum                 |]
  , goldens "maybeObj"                 [AllTypes.get| result.maybeObject          |]
  , goldens "maybeObj_bang"            [AllTypes.get| result.maybeObject!         |]
  , goldens "maybeObj_text"            [AllTypes.get| result.maybeObject.text     |]
  , goldens "maybeObj_bang_text"       [AllTypes.get| result.maybeObject!.text    |]
  , goldens "maybeObjNull"             [AllTypes.get| result.maybeObjectNull      |]
  , goldens "maybeObjNull_text"        [AllTypes.get| result.maybeObjectNull.text |]
  , goldens "maybeList"                [AllTypes.get| result.maybeList            |]
  , goldens "maybeList_bang"           [AllTypes.get| result.maybeList!           |]
  , goldens "maybeList_bang_list"      [AllTypes.get| result.maybeList![]         |]
  , goldens "maybeList_bang_list_text" [AllTypes.get| result.maybeList![].text    |]
  , goldens "maybeList_list"           [AllTypes.get| result.maybeList[]          |]
  , goldens "maybeList_list_text"      [AllTypes.get| result.maybeList[].text     |]
  , goldens "maybeListNull"            [AllTypes.get| result.maybeListNull        |]
  , goldens "maybeListNull_list"       [AllTypes.get| result.maybeListNull[]      |]
  , goldens "maybeListNull_list_text"  [AllTypes.get| result.maybeListNull[].text |]
  , goldens "list"                     [AllTypes.get| result.list                 |]
  , goldens "list_explicit"            [AllTypes.get| result.list[]               |]
  , goldens "list_type"                [AllTypes.get| result.list[].type          |]
  , goldens "list_maybeBool"           [AllTypes.get| result.list[].maybeBool     |]
  , goldens "list_maybeInt"            [AllTypes.get| result.list[].maybeInt      |]
  , goldens "nonexistent"              [AllTypes.get| result.nonexistent          |]
  ]
  where
    result = AllTypes.result

testKeepSchemaAllTypes :: TestTree
testKeepSchemaAllTypes = goldens "keep_schema_all_types" $ map fromObj list
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

-- | Kept schemas can have the same name for different Results. Here, two schemas are stored with
-- the name "o", but one is stored for AllTypes and the other is stored for Nested.
testKeepSchemaNamespaced :: TestTree
testKeepSchemaNamespaced = goldens "keep_schema_namespaced" $
  map fromAllTypes allTypesList ++ map fromNested nestedList
  where
    allTypes = AllTypes.result
    nested = Nested.result
    allTypesList = [AllTypes.get| allTypes.list[] > o |]
    nestedList = [Nested.get| nested.list[] > o |]
    fromAllTypes o = Text.unpack [AllTypes.get| @o.type |]
    fromNested o = show [Nested.get| @o.b |]

testInvalidGetters :: TestTree
testInvalidGetters = testGroup "Test invalid getters"
  [ badGoldens "unstored_schema" "@asdf.foo.bar"
  , badGoldens "reference_stored" "node.a"
  , badGoldens "store_duplicate" "result.maybeObject > node"
  , badGoldens "store_not_object" "result.bool > bool"
  , badGoldens "invalid_key" "result.foo"
  , badGoldens "invalid_type_key" "result.bool.foo"
  , badGoldens "invalid_bang" "result.bool!"
  , badGoldens "invalid_list" "result.maybeObject[]"
  ]
  where
    badGoldens name input = goldens' name $
      try @SomeException (runQ $ generateGetter input) >>=
        either return (\_ -> fail "Invalid getter incorrectly parsed the input")
    generateGetter = generateGetter' getQ putQ 'AllTypes.UnsafeResult AllTypes.schema
    getQ = pure $ Just [("AllTypes.UnsafeResult$node", SchemaObject [("a", SchemaText)])]
    putQ = runIO . print
