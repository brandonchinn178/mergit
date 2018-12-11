{-|
Module      :  Data.GraphQL.TH
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for querying GraphQL results using quasiquoters.

'Data.GraphQL.Schema.Schema' defines the shape of the JSON object stored in
'Data.GraphQL.Schema.Object', and we can use 'Data.GraphQL.Schema.getKey' to lookup a key that is
checked at compile-time to exist in the object.

To make it easier to extract deeply nested keys, this module defines QuasiQuoters that generate the
corresponding 'Data.GraphQL.Schema.getKey' expressions.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.GraphQL.TH
  ( get
  , getter
  ) where

import Control.Monad ((>=>))
import Data.List (uncons)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.GraphQL.Schema (Object, SchemaGraph(..), ToEnum, ToScalar, getKey)
import Data.GraphQL.TH.Parse

-- | Defines a QuasiQuoter for expressions.
--
-- > doFoo = do
-- >   result <- runQuery ...
-- >
-- >   [get| result.foo.a |]          :: Int
-- >   [get| result.foo.nodes |]      :: [Object (..)]
-- >   [get| result.foo.nodes[] |]    :: [Object (..)]
-- >   [get| result.foo.nodes[].b |]  :: [Maybe Bool]
-- >   [get| result.foo.nodes[].b! |] :: [Bool] -- runtime error if any "b" values are null
-- >   [get| result.foo.c |]          :: Text
-- >   [get| result.foo.(a,c) |]      :: (Int, Text)
-- >   [get| result.foo.[c,d] |]      :: [Text]
-- >
-- >   let nodes = [get| result.foo.nodes |]
-- >   flip map nodes $ \node -> fromMaybe ([get| node.num |] == 0) [get| node.b |]
-- >   map [get| .num |] nodes
--
-- These "getter" expressions follow the given rules:
--
-- * @x@ returns the value of @x@ with the given type:
--
--     * @SchemaBool@ returns a 'Bool'
--     * @SchemaInt@ returns an 'Int'
--     * @SchemaDouble@ returns a 'Double'
--     * @SchemaText@ returns a 'Text.Text'
--     * @SchemaScalar name@ returns a value of the type associated with the given name
--     * @SchemaEnum name@ returns a value of the type associated with the given name
--     * @SchemaMaybe schema@ returns a 'Maybe' value wrapping the value returned by the inner schema
--     * @SchemaList schema@ returns a list of values, whose type is determined by the inner schema
--     * @SchemaObject fields@ returns a 'Data.GraphQL.Schema.Object'
--
-- * @x.y@ is only valid if @x@ is a @SchemaObject@ or a @SchemaMaybe SchemaObject@. Returns the value
--   of the key @y@ in the (potentially wrapped) 'Object'.
--
-- * @x.[y,z]@ is only valid if @x@ is a @SchemaObject@ or a @SchemaMaybe SchemaObject@, and if @y@ and
--   @z@ have the same schema. Returns the value of the keys @y@ and @z@ in the (potentially wrapped)
--   'Object' as a list.
--
-- * @x.(y,z)@ is only valid if @x@ is a @SchemaObject@ or a @SchemaMaybe SchemaObject@. Returns the
--   value of the keys @y@ and @z@ in the (potentially wrapped) 'Object' as a tuple.
--
-- * @x!@ is only valid if @x@ is a @SchemaMaybe@. Unwraps the value of @x@ from a 'Just' value and
--   errors (at runtime!) if @x@ is 'Nothing'.
--
-- * @x[]@ is only valid if @x@ is a @SchemaList@. Applies the remaining rules as an 'fmap' over the
--   values in the list.
--
--     * @x[]@ without anything after is equivalent to @x@
--     * @x[].y@ gets the key @y@ in all the Objects in @x@
--     * @x[]!@ unwraps all 'Just' values in @x@ (and errors if any 'Nothing' values exist in @x@)
--
-- * @x?@ follows the same rules as @x[]@ except it's only valid if @x@ is a @SchemaMaybe@.
get :: QuasiQuoter
get = QuasiQuoter
  { quoteExp = parse getterExp >=> generateGetterExp
  , quoteDec = error "Cannot use `get` for Dec"
  , quoteType = error "Cannot use `get` for Type"
  , quotePat = error "Cannot use `get` for Pat"
  }

generateGetterExp :: GetterExp -> ExpQ
generateGetterExp GetterExp{..} =
  case start of
    Nothing -> do
      arg <- newName "x"
      lamE [varP arg] (apply arg)
    Just arg -> apply $ mkName arg
  where
    apply = appE (mkGetter getterOps) . varE
    mkGetter [] = [| id |]
    mkGetter (op:ops) =
      let next = mkGetter ops
      in case op of
        GetterKey key ->
          let getKey' = appTypeE [|getKey|] (litT $ strTyLit key)
          in [| $(next) . $(getKey') |]
        GetterKeyList inner -> do
          val <- newName "v"
          lamE [varP val] (listE $ applyValToOps val inner)
        GetterKeyTuple inner -> do
          val <- newName "v"
          lamE [varP val] (tupE $ applyValToOps val inner)
        GetterBang -> [| $(next) . fromJust |]
        GetterMapMaybe -> [| ($(next) <$?>) |]
        GetterMapList -> [| ($(next) `map`) |]
    applyValToOps val ops = map ((`appE` varE val) . mkGetter) ops

(<$?>) :: (a -> b) -> Maybe a -> Maybe b
(<$?>) = (<$>)

-- | Defines a QuasiQuoter to generate a getter function and type alias for a given expression.
--
-- For example, the following code
--
-- > type MySchema = 'SchemaObject '[ '("foo", 'SchemaObject '[ '("bar", 'SchemaText) ]) ]
-- >
-- > [getter| MySchema > foo > MyFoo |]
--
-- generates a type @MyFoo@ and a function @getMyFoo@. @getMyFoo@ is defined to be @[get| .foo |]@
-- (if it were called on an 'Object' with the schema @MySchema@) and @MyFoo@ is the resulting type
-- of @getMyFoo@; in this case, @Object ('SchemaObject '[ '("bar", 'SchemaInt) ])@.
--
-- > parseBar :: Foo -> [Text]
-- > parseBar = Text.splitOn "," . [get| .bar |]
getter :: QuasiQuoter
getter = QuasiQuoter
  { quoteExp = error "Cannot use `getter` for Exp"
  , quoteDec = parse getterDecs >=> generateGetterDecs
  , quoteType = error "Cannot use `getter` for Type"
  , quotePat = error "Cannot use `getter` for Pat"
  }

generateGetterDecs :: GetterDecs -> DecsQ
generateGetterDecs GetterDecs{..} = do
  getterFuncName <- newName $ "get" ++ endSchema
  endSchemaName <- newName endSchema
  startSchemaName <- maybe (fail $ "Unknown schema: " ++ startSchema) return =<< lookupTypeName startSchema
  startSchema' <- reify startSchemaName >>= \case
    TyConI (TySynD _ _ ty) -> return ty
    info -> fail $ "Unknown type to generate getter function for: " ++ show info
  let getterType = tySynD endSchemaName [] $ fromSchemaType $ fromOps startSchema' getterOps
      getterBody = generateGetterExp $ GetterExp Nothing getterOps
      getterFunc = funD getterFuncName [clause [] (normalB getterBody) []]
  sequence [getterType, getterFunc]
  where
    unSig = \case
      SigT ty _ -> ty
      ty -> ty
    fromOps = foldl getType
    getType schema op = case unSig schema of
      AppT (PromotedT ty) inner ->
        case op of
          GetterKey key | ty == 'SchemaObject ->
            fromMaybe (error $ "Key '" ++ key ++ "' does not exist in schema: " ++ show schema)
            $ lookup key $ getObjectSchema inner
          GetterKey key -> error $ "Cannot get key '" ++ key ++ "' in schema: " ++ show schema
          GetterKeyList elems | ty == 'SchemaObject ->
            let (elemType, rest) = fromJust $ uncons $ map (fromOps schema) elems
            in if all (== elemType) rest
              then elemType -- return the wrapped type
              else error $ "List contains different types with schema: " ++ show schema
          GetterKeyList _ -> error $ "Cannot get keys in schema: " ++ show schema
          GetterKeyTuple elems | ty == 'SchemaObject ->
            foldl (\acc ops -> AppT acc $ fromOps schema ops) (TupleT $ length elems) elems
          GetterKeyTuple _ -> error $ "Cannot get keys in schema: " ++ show schema
          GetterBang | ty == 'SchemaMaybe -> inner
          GetterBang -> error $ "Cannot use `!` operator on schema: " ++ show schema
          GetterMapMaybe | ty == 'SchemaMaybe -> inner -- return the wrapped type
          GetterMapMaybe -> error $ "Cannot use `?` operator on schema: " ++ show schema
          GetterMapList | ty == 'SchemaList -> inner -- return the wrapped type
          GetterMapList -> error $ "Cannot use `[]` operator on schema: " ++ show schema
      _ -> error $ unlines ["Cannot get type:", show schema, show op]
    getObjectSchema schema = case unSig schema of
      AppT (AppT PromotedConsT t1) t2 ->
        case unSig t1 of
          AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit key))) ty -> (key, ty) : getObjectSchema t2
          _ -> error $ "Could not parse a (key, schema) tuple: " ++ show t1
      PromotedNilT -> []
      t -> error $ "Could not get object schema: " ++ show t
    fromSchemaType schema = case unSig schema of
      AppT (PromotedT ty) inner
        | ty == 'SchemaScalar -> [t| ToScalar $(pure inner) |]
        | ty == 'SchemaEnum -> [t| ToEnum $(pure inner) |]
        | ty == 'SchemaMaybe -> [t| Maybe $(fromSchemaType inner) |]
        | ty == 'SchemaList -> [t| [$(fromSchemaType inner)] |]
        | ty == 'SchemaObject -> [t| Object $(pure schema) |]
      PromotedT ty
        | ty == 'SchemaBool -> [t| Bool |]
        | ty == 'SchemaInt -> [t| Int |]
        | ty == 'SchemaDouble -> [t| Double |]
        | ty == 'SchemaText -> [t| Text |]
      AppT t1 t2 -> appT (fromSchemaType t1) (fromSchemaType t2)
      TupleT _ -> pure schema
      _ -> error $ "Could not convert schema: " ++ show schema
