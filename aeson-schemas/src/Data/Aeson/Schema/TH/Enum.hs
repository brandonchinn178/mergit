{-|
Module      :  Data.Aeson.Schema.TH.Enum
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Template Haskell functions for Enum types.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aeson.Schema.TH.Enum
  ( genFromJSONEnum
  ) where

import Control.Monad (unless)
import Data.Aeson (FromJSON(..), Value(..))
import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Language.Haskell.TH

-- | Generate an instance of 'FromJSON' for the given data type.
--
-- The given data type MUST be an instance of 'Enum'.
--
-- The 'FromJSON' instance will match to a string value matching the constructor name,
-- case-insensitive.
--
-- @
-- data State = OPEN | CLOSED deriving (Show,Enum)
--
-- genFromJSONEnum ''State
--
-- main = print
--   [ decode \"open" :: Maybe State
--   , decode \"OPEN" :: Maybe State
--   , decode \"closed" :: Maybe State
--   , decode \"CLOSED" :: Maybe State
--   ]
-- @
genFromJSONEnum :: Name -> Q [Dec]
genFromJSONEnum name = do
  isEnum name >>= flip unless (fail $ "Not an Enum type: " ++ show name)

  matches <- reify name >>= \case
    TyConI (DataD _ _ _ _ cons _) -> return $ flip map cons $ \case
      NormalC con [] ->
        let pat = litP . stringL . map toLower . nameBase $ con
        in match pat (normalB [| pure $(conE con) |]) []
      con -> fail $ "Invalid constructor: " ++ show con
    info -> fail $ "Invalid data type: " ++ show info

  t <- newName "t"
  let parseEnum = caseE [| Text.unpack $ Text.toLower $(varE t) |] $
        matches ++ [match wildP (normalB $ appE badParse $ varE t) []]

  [d|
    instance FromJSON $(conT name) where
      parseJSON (String $(varP t)) = $parseEnum
      parseJSON v = $badParse v
    |]
  where
    badParse =
      let prefix = litE $ stringL $ "Bad " ++ nameBase name ++ ": "
      in [| fail . ($prefix ++) . show |]

{- Helpers -}

isEnum :: Name -> Q Bool
isEnum name = do
  ClassI _ instances <- reify ''Enum
  let instanceNames = flip mapMaybe instances $ \case
        InstanceD _ _ (AppT _ (ConT n)) _ -> Just n
        _ -> Nothing
  return $ name `elem` instanceNames
