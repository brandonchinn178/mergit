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

-- | TODO
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
