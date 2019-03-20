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
import Data.Aeson.Schema.TH.Parse
import Data.Aeson.Schema.TH.Unwrap (getType)
import Data.Aeson.Schema.TH.Utils (reifySchema)

-- | TODO
mkGetter :: String -> String -> Name -> String -> DecsQ
mkGetter unwrapName funcName startSchemaName ops = do
  -- TODO: allow (Object schema)
  startSchemaType <- reifySchema startSchemaName

  getterExp'@GetterExp{..} <- parse getterExp ops
  unless (isNothing start) $
    fail $ "Getter expression should start with '.': " ++ ops

  let unwrapResult = getType startSchemaType getterOps
      getterFunc = generateGetterExp getterExp'
      unwrapName' = mkName unwrapName
      funcName' = mkName funcName

  sequence
    [ tySynD unwrapName' [] unwrapResult
    -- FIXME: unwrapResult should be result of unwrapping in context of 'get' function
    , sigD funcName' [t| Object $(pure startSchemaType) -> $unwrapResult |]
    , funD funcName' [clause [] (normalB getterFunc) []]
    ]
