{-|
Module      :  Data.GraphQL.Aeson
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Helpers for working with the Aeson library.
-}

module Data.GraphQL.Aeson
  ( object
  , fromScientific
  , fromScientific'
  , toInt
  , toDouble
  -- * Re-exports
  , Value(..)
  , (.=)
  ) where

import Data.Aeson hiding (object)
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific (Scientific, floatingOrInteger)

-- | A helper to convert pairs into an 'Object'.
--
-- > object [ "a" .= 1 ]
object :: [Pair] -> Object
object = HashMap.fromList

-- | An alias for 'floatingOrInteger'.
fromScientific :: (RealFloat r, Integral i) => Scientific -> Either r i
fromScientific = floatingOrInteger

-- | 'fromScientific' specialized to 'Double' and 'Int'.
fromScientific' :: Scientific -> Either Double Int
fromScientific' = fromScientific

-- | Convert 'Scientific' into 'Int'.
toInt :: Scientific -> Maybe Int
toInt = either (const Nothing) Just . fromScientific'

-- | Convert 'Scientific' into 'Double'.
toDouble :: Scientific -> Maybe Double
toDouble = either Just (const Nothing) . fromScientific'
