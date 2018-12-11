{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Nested where

import Data.GraphQL (SchemaGraph(..))
import Data.GraphQL.Schema.Internal (Object(..))
import Util (getMockedResult)

type Schema = 'SchemaObject
  '[ '("list", 'SchemaList ('SchemaObject
        '[ '("a", 'SchemaMaybe ('SchemaObject
              '[ '("b", 'SchemaInt)
               ]
            ))
         , '("b", 'SchemaInt)
         ]
      ))
   ]

result :: Object Schema
result = $(getMockedResult "test/nested.json")
