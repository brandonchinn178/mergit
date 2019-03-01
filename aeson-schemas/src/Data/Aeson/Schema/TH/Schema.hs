{-|
Module      :  Data.Aeson.Schema.TH.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

The 'schema' quasiquoter.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aeson.Schema.TH.Schema (schema) where

import Control.Monad ((>=>))
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Data.Aeson.Schema.Internal (SchemaGraph(..))
import Data.Aeson.Schema.TH.Parse

-- | Defines a QuasiQuoter for writing schemas.
--
-- Example:
--
-- > import Data.Aeson.Schema (SchemaGraph(..))
-- > import Data.Aeson.Schema.TH (schema)
-- >
-- > -- | MySchema ~ 'SchemaObject
-- > --     '[ '("a", 'SchemaInt)
-- > --      , '("nodes", 'SchemaMaybe (SchemaList ('SchemaObject
-- > --           '[ '("b", 'SchemaMaybe 'SchemaBool)
-- > --            ]
-- > --         )))
-- > --      , '("c", 'SchemaText)
-- > --      , '("d", 'SchemaText)
-- > --      , '("e", 'SchemaCustom "MyType")
-- > --      ]
-- > type MySchema = [schema|
-- >   {
-- >     "foo": {
-- >        "a": Int,
-- >        "nodes": Maybe List {
-- >           "b": Maybe Bool,
-- >        },
-- >        "c": Text,
-- >        "d": Text,
-- >        "e": MyType,
-- >     },
-- >   }
-- > |]
--
-- The schema definition accepts the following syntax:
--
-- * @Bool@ corresponds to @'SchemaBool@, and similarly for @Int@, @Double@, and @Text@
--
-- * @Maybe x@ and @List x@ correspond to @'SchemaMaybe x@ and @'SchemaList x@, respectively. (no
--   parentheses needed)
--
-- * Any other uppercase identifier corresponds to @'SchemaCustom ident@
--
-- * @{ "key": schema, ... }@ corresponds to @'SchemaObject '[ '("key", schema), ... ]@
--
-- * @{ "key": #Other }@ includes @Other@ as a schema
--
-- * @{ "key": schema, #Other }@ extends this schema with @Other@
schema :: QuasiQuoter
schema = QuasiQuoter
  { quoteExp = error "Cannot use `schema` for Exp"
  , quoteDec = error "Cannot use `schema` for Dec"
  , quoteType = parse schemaDef >=> generateSchema
  , quotePat = error "Cannot use `schema` for Pat"
  }

generateSchema :: SchemaDef -> TypeQ
generateSchema = \case
  SchemaDefType "Bool"       -> [t| 'SchemaBool |]
  SchemaDefType "Int"        -> [t| 'SchemaInt |]
  SchemaDefType "Double"     -> [t| 'SchemaDouble |]
  SchemaDefType "Text"       -> [t| 'SchemaText |]
  SchemaDefType other        -> [t| 'SchemaCustom $(strLitT other) |]
  SchemaDefMod "Maybe" inner -> [t| 'SchemaMaybe $(generateSchema inner) |]
  SchemaDefMod "List" inner  -> [t| 'SchemaList $(generateSchema inner) |]
  SchemaDefMod other _       -> fail $ "Invalid schema modification: " ++ other
  SchemaDefObj pairs         -> [t| 'SchemaObject $(fromPairs pairs) |]
  SchemaDefInclude other     -> maybe (fail $ "Unknown type: " ++ other) conT =<< lookupTypeName other
  where
    strLitT = litT . strTyLit
    pairT (a, b) = [t| '($a, $b) |]
    consT x xs = [t| $x ': $xs |]
    fromPairs = \case
      [] -> [t| '[] |]
      (k, v):rest -> pairT (strLitT k, generateSchema v) `consT` fromPairs rest
