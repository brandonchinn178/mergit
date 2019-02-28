{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Nested where

import Data.GraphQL (Object, SchemaGraph(..))
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
