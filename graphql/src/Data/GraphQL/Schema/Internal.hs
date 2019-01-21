{-|
Module      :  Data.GraphQL.Schema.Internal
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for GraphQL response objects and the schema encoded in it.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Data.GraphQL.Schema.Internal where

import qualified Data.Aeson as Aeson
import Data.List (intercalate, isPrefixOf)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, splitTyConApp, tyConName, typeRep, typeRepTyCon)
import GHC.TypeLits (Symbol)

-- | The object returned by a GraphQL query, containing the schema of the JSON object.
newtype Object (schema :: SchemaType) = UnsafeObject Aeson.Object
  deriving (Show)

{- Schema definition -}

-- | The schema definition for a GraphQL result.
data SchemaGraph s
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaScalar s
  | SchemaEnum s
  | SchemaMaybe (SchemaGraph s)
  | SchemaList (SchemaGraph s)
  | SchemaObject [(s, SchemaGraph s)]
  deriving (Show)

-- | A 'SchemaGraph' at the kind level.
type SchemaType = SchemaGraph Symbol

-- | Pretty show the given SchemaType.
prettyShow :: forall (a :: SchemaType). Typeable a => String
prettyShow = showSchemaType $ typeRep (Proxy @a)
  where
    showSchemaType tyRep = case splitTypeRep tyRep of
      ("'SchemaObject", [pairs]) ->
        unwords ["SchemaObject", showPairs $ getSchemaObjectPairs pairs]
      (con, args) | "'Schema" `isPrefixOf` con ->
        unwords $ tail con : map (wrap . showSchemaType) args
      (con, _) -> con
    getSchemaObjectPairs tyRep = case splitTypeRep tyRep of
      ("'[]", []) -> []
      ("':", [x, rest]) -> case splitTypeRep x of
        ("'(,)", [key, val]) -> (typeRepName key, typeRepName val) : getSchemaObjectPairs rest
        _ -> error $ "Unknown pair when showing SchemaType: " ++ show x
      _ -> error $ "Unknown list when showing SchemaType: " ++ show tyRep
    showPairs l =
      let showPair (a, b) = "(" ++ a ++ ", " ++ b ++ ")"
      in "[" ++ intercalate ", " (map showPair l) ++ "]"
    wrap s = if ' ' `elem` s then "(" ++ s ++ ")" else s
    splitTypeRep rep =
      let (con, args) = splitTyConApp rep
      in (tyConName con, args)
    typeRepName = tyConName . typeRepTyCon
