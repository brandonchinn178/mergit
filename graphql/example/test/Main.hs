{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.GraphQL (QuerySettings(..), defaultQuerySettings, runQueryT)
import Data.GraphQL.Aeson (Value, fromObject)
import Data.GraphQL.TestUtils (matches, mockWith)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Example (App(..), searchForSong)
import qualified Recordings

data MockData = MockData
  { mockRecordings :: String -> Value
  }

runMockApp :: MockData -> App a -> IO a
runMockApp mocked = runQueryT (mockedQuerySettings $ withMock mocked) . unApp
  where
    withMock MockData{..} =
      [ (matches Recordings.query, mockRecordings . fromJust . fromObject "query")
      ]

goldens :: Show a => String -> IO a -> TestTree
goldens name = goldenVsString name fp . fmap (ByteString.pack . show)
  where
    fp = "test/goldens/" ++ name ++ ".golden"

mock1 :: MockData
mock1 = MockData
  { mockRecordings = \title ->
      let artist = "Foo"
          title' = fromIntegral $ length title
          noVowels = filter (`notElem` ("aeiouAEIOU" :: String)) title
          noVowels' = fromIntegral $ length noVowels
          duration = title' / noVowels' * 180000 :: Double
          voteCount = title' * noVowels' :: Double
          value = noVowels' / title' * 5 :: Double
      in
        [aesonQQ|
          {
            "search": {
              "recordings": {
                "nodes": [
                  {
                    "title": #{title},
                    "artists": {
                      "nodes": [
                        {
                          "name": #{artist}
                        }
                      ]
                    },
                    "video": true,
                    "length": #{duration},
                    "rating": {
                      "voteCount": #{voteCount},
                      "value": #{value}
                    },
                    "releases": {
                      "nodes": [
                        {
                          "title": #{"The Best of " ++ artist},
                          "date": "2000",
                          "status": "OFFICIAL"
                        }
                      ]
                    }
                  }
                ]
              }
            }
          }
        |]
  }

main :: IO ()
main = defaultMain $ testGroup "graphql-example-test"
  [ goldens "searchForSong_AllStar" $ runMockApp mock1 $ searchForSong "All Star"
  , goldens "searchForSong_GangnamStyle" $ runMockApp mock1 $ searchForSong "Gangnam Style"
  ]
