{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import GHC.Exception (ErrorCall(..))
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

goldens' :: String -> IO String -> TestTree
goldens' name = goldenVsString name fp . fmap ByteString.pack
  where
    fp = "test/goldens/" ++ name ++ ".golden"

goldens :: Show s => String -> s -> TestTree
goldens name = goldens' name . pure . show

testValidGetters :: TestTree
testValidGetters = testGroup "Test valid getters"
  [ goldens "bool"                     [AllTypes.get| result.bool                 |]
  , goldens "int"                      [AllTypes.get| result.int                  |]
  , goldens "int_int2"                 [AllTypes.get| result.[int,int2]           |]
  , goldens "double"                   [AllTypes.get| result.double               |]
  , goldens "bool_int_double"          [AllTypes.get| result.(bool,int,double)    |]
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
    fromObj obj = case [Nested.get| @o obj.a > field |] of
      Just field -> [Nested.get| @field.b |]
      Nothing    -> [Nested.get| @o obj.b |]

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
      try (runQ $ generateGetter input) >>= \case
        Right _ -> fail "Invalid getter incorrectly parsed the input"
        Left (ErrorCall msg) -> return msg
    generateGetter = generateGetter' getQ putQ 'AllTypes.UnsafeResult AllTypes.schema
    getQ = pure $ Just [("AllTypes.UnsafeResult$node", SchemaObject [("a", SchemaText)])]
    putQ = runIO . print
