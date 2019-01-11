{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Example.Test.Mock where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Aeson.QQ (aesonQQ)
import Data.GraphQL.Aeson (fromObject')
import Data.GraphQL.TestUtils (MocksApi(..), mock)
import Data.List (isInfixOf)

import Example.GraphQL.API (API)
import qualified Example.GraphQL.Recordings as Recordings

-- | A Haskell representation of a Recording stored in the database.
newtype Recording = Recording { recordName :: String }
  deriving (Show)

instance ToJSON Recording where
  toJSON (Recording title) =
    [aesonQQ|
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
    |]
    where
      artist = "Foo"
      titleLen = fromIntegral $ length title
      noVowels = filter (`notElem` ("aeiouAEIOU" :: String)) title
      noVowels' = fromIntegral $ length noVowels
      duration = titleLen / noVowels' * 180000 :: Double
      voteCount = titleLen * noVowels' :: Double
      value = noVowels' / titleLen * 5 :: Double

encodeRecordings :: [Recording] -> String -> Value
encodeRecordings recordings title = object
  [ "search" .= object
    [ "recordings" .= object
      [ "nodes" .= filter ((title `isInfixOf`) . recordName) recordings
      ]
    ]
  ]

data MockData = MockData
  { mockRecordings :: [Recording]
  }

-- | Default mock data.
mockData :: MockData
mockData = MockData
  { mockRecordings = []
  }

instance MocksApi API MockData where
  mockWith MockData{..} =
    [ (mock Recordings.query, encodeRecordings mockRecordings . fromObject' "query")
    ]
