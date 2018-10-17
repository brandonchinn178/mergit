{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.GraphQL (QuerySettings(..), Value, defaultQuerySettings, runQueryT)
import Data.GraphQL.TestUtils (matches, mockWith)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified Countries
import Example (App(..), getCountries)

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

main :: IO ()
main = defaultMain $ testGroup "graphql-example-test"
  [ goldens "mock1" $
      let mock = MockData
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
      in runMockApp mock getCountries
  , goldens "mock2" $
      let mock = MockData
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
      in runMockApp mock getCountries
  ]
