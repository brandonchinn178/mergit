{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nested where

import Data.Aeson (decodeFileStrict)
import qualified Data.Maybe as Maybe
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (lift)

import Data.GraphQL

newtype Result = UnsafeResult Value

schema :: Schema
schema = SchemaObject
  [ ("list", SchemaList $ SchemaObject
      [ ("a", SchemaMaybe $ SchemaObject [("b", SchemaInt)])
      , ("b", SchemaInt)
      ]
    )
  ]

get :: QuasiQuoter
get = getterFor 'UnsafeResult schema

result :: Result
result = $(do
  obj <- runIO $ decodeFileStrict "test/nested.json"
  [| UnsafeResult $(lift $ Maybe.fromJust (obj :: Maybe Value)) |]
  )
