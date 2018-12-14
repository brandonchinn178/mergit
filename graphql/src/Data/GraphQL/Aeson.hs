{-|
Module      :  Data.GraphQL.Aeson
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Helpers for working with the Aeson library.
-}

module Data.GraphQL.Aeson
  ( object
  , fromObject
  , fromScientific
  , fromScientific'
  , toInt
  , toDouble
  -- * Re-exports
  , Value(..)
  , Object
  , (.=)
  ) where

import Control.Monad ((<=<))
import Data.Aeson hiding (object)
import Data.Aeson.Types (Pair)
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)

-- | A helper to convert pairs into an 'Object'.
--
-- > object [ "a" .= 1 ]
object :: [Pair] -> Object
object = HashMap.fromList

-- | A helper to parse values from an 'Object'.
fromObject :: FromJSON a => Text -> Object -> Maybe a
fromObject key = resultToMaybe . fromJSON <=< HashMap.lookup key
  where
    resultToMaybe (Error _) = Nothing
    resultToMaybe (Success a) = Just a

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
