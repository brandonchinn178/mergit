{-|
Module      :  Data.Aeson.Schema.TH.Getter
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Template Haskell functions for getter functions.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.TH.Getter where

import Control.Monad (unless)
import Data.Aeson.Schema.Internal (Object)
import Data.Maybe (isNothing)
import Language.Haskell.TH

import Data.Aeson.Schema.TH.Get (generateGetterExp)
import Data.Aeson.Schema.TH.Parse (GetterExp(..), getterExp, parse)
import Data.Aeson.Schema.TH.Utils (reifySchema, unwrapType)

-- | A helper that generates a 'get' expression and a type alias for the result of the expression.
--
-- > mkGetter "Node" "getNodes" ''MySchema ".nodes![]"
-- >
-- > -- generates roughly:
-- > type Node = Object ('SchemaObject '[ '("b", 'SchemaMaybe 'SchemaBool) ])
-- > getNodes :: Object MySchema -> [Node]
-- > getNodes = [get| .nodes![] |]
--
-- 'mkGetter' takes four arguments:
--
--   [@unwrapName@] The name of the type synonym to store the unwrapped schema as
--
--   [@funcName@] The name of the getter function
--
--   [@startSchema@] The schema to extract/unwrap from
--
--   [@ops@] The operation to extract/unwrap from the schema
--
-- @ops@ is passed to the 'unwrap' and 'get' quasiquoters to create the unwrapped schema and the
-- getter function. There is one subtlety that occurs from the use of the same @ops@ string for
-- both the 'unwrap' and 'get' quasiquoters: 'unwrap' strips out intermediate functors, while 'get'
-- applies within the functor. So in the above example, @".nodes![]"@ strips out the list when
-- saving the schema to @Node@, while in the below example, @".nodes!"@ doesn't strip out the list
-- when saving the schema to @Nodes@.
--
-- > mkGetter "Nodes" "getNodes" ''MySchema ".nodes!"
-- >
-- > -- generates roughly:
-- > type Nodes = [Object ('SchemaObject '[ '("b", 'SchemaMaybe 'SchemaBool) ])]
-- > getNodes :: Object MySchema -> Nodes
-- > getNodes = [get| .nodes! |]
--
-- As another example,
--
-- > mkGetter "MyBool" "getMyBool" ''MySchema ".nodes?[].b"
-- >
-- > -- generates roughly:
-- > type MyBool = Maybe Bool
-- > getMyBool :: Object MySchema -> Maybe [MyBool]
-- > getMyBool = [get| .nodes?[].b |]
mkGetter :: String -> String -> Name -> String -> DecsQ
mkGetter unwrapName funcName startSchemaName ops = do
  -- TODO: allow (Object schema)
  startSchemaType <- reifySchema startSchemaName

  getterExp'@GetterExp{..} <- parse getterExp ops
  unless (isNothing start) $
    fail $ "Getter expression should start with '.': " ++ ops

  let unwrapResult = unwrapType False getterOps startSchemaType
      funcResult = unwrapType True getterOps startSchemaType
      getterFunc = generateGetterExp getterExp'
      unwrapName' = mkName unwrapName
      funcName' = mkName funcName

  sequence
    [ tySynD unwrapName' [] unwrapResult
    , sigD funcName' [t| Object $(pure startSchemaType) -> $funcResult |]
    , funD funcName' [clause [] (normalB getterFunc) []]
    ]
