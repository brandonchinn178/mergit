{-|
Module      :  Data.Aeson.Schema.TH.Utils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Schema.TH.Utils
  ( stripSigs
  ) where

import Language.Haskell.TH

-- | Strip all kind signatures from the given type.
stripSigs :: Type -> Type
stripSigs = \case
  ForallT tyVars ctx ty -> ForallT tyVars ctx (stripSigs ty)
  AppT ty1 ty2 -> AppT (stripSigs ty1) (stripSigs ty2)
  SigT ty _ -> stripSigs ty
  InfixT ty1 name ty2 -> InfixT (stripSigs ty1) name (stripSigs ty2)
  UInfixT ty1 name ty2 -> UInfixT (stripSigs ty1) name (stripSigs ty2)
  ParensT ty -> ParensT (stripSigs ty)
  ty -> ty
