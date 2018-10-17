{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.GraphQL (QuerySettings(..), Value, defaultQuerySettings, runQueryT)
import Data.GraphQL.TestUtils (matches, mockWith)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified Countries
import Example (App(..), getContinents, getCountries)

data MockData = MockData
  { mockCountries :: Value
  }

runMockApp :: MockData -> App a -> IO a
runMockApp MockData{..} = runQueryT querySettings . unApp
  where
    querySettings = defaultQuerySettings
      { mockResponse = mockWith
          [ (matches Countries.query, const mockCountries)
          ]
      }

goldens :: Show a => String -> IO a -> TestTree
goldens name = goldenVsString name fp . fmap (ByteString.pack . show)
  where
    fp = "test/goldens/" ++ name ++ ".golden"

mock1 :: MockData
mock1 = MockData
  { mockCountries = [aesonQQ|
      {
        "countries": [
          {
            "name": "Foo",
            "continent": {
              "code": "EU"
            }
          }
        ]
      }
    |]
  }

mock2 :: MockData
mock2 = MockData
  { mockCountries = [aesonQQ|
      {
        "countries": [
          {
            "name": "Bar",
            "continent": {
              "code": "NA"
            }
          },
          {
            "name": "Baz",
            "continent": {
              "code": "SA"
            }
          },
          {
            "name": "Quux",
            "continent": {
              "code": "SA"
            }
          }
        ]
      }
    |]
  }

main :: IO ()
main = defaultMain $ testGroup "graphql-example-test"
  [ goldens "mockContinents1" $ runMockApp mock1 getContinents
  , goldens "mockContinents2" $ runMockApp mock2 getContinents
  , goldens "mockCountries1" $ runMockApp mock1 getCountries
  , goldens "mockCountries2" $ runMockApp mock2 getCountries
  ]
