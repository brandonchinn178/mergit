{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Example where

import Control.Monad.IO.Class (MonadIO)
import Data.GraphQL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import qualified Countries

newtype App a = App { unApp :: QueryT IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadQuery)

runApp :: App a -> IO a
runApp = runQueryT querySettings . unApp
  where
    querySettings = defaultQuerySettings
      { url = "https://countries.trevorblades.com/"
      }

data Continent
  = Europe
  | Asia
  | Africa
  | Antarctica
  | NorthAmerica
  | SouthAmerica
  | Oceania
  deriving (Show,Eq,Ord)

fromCode :: Text -> Continent
fromCode = \case
  "EU" -> Europe
  "AS" -> Asia
  "AF" -> Africa
  "AN" -> Antarctica
  "NA" -> NorthAmerica
  "SA" -> SouthAmerica
  "OC" -> Oceania
  code -> error $ "Invalid code: " ++ show code

getCountries :: App [Text]
getCountries = do
  result <- runQuery Countries.query Countries.Args
  return [Countries.get| result.countries[].name |]

getContinents :: App (Map Continent [Text])
getContinents = do
  result <- runQuery Countries.query Countries.Args
  let countries = [Countries.get| result.countries[] > country |]
      fromCountry country =
        ( fromCode [Countries.get| country.continent.code |]
        , [Countries.get| country.name |]
        )
  return . Map.fromListWith (++) . map (listSnd . fromCountry) $ countries
  where
    listSnd (a, b) = (a, [b])
