import qualified Data.ByteString.Lazy.Char8 as ByteString
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Example (searchForSong)
import Example.Test.Mock
import Example.Test.Monad

goldens :: Show a => String -> IO a -> TestTree
goldens name = goldenVsString name fp . fmap (ByteString.pack . show)
  where
    fp = "test/goldens/" ++ name ++ ".golden"

main :: IO ()
main = defaultMain $ testGroup "graphql-example-test"
  [ goldens "searchForSong_AllStar" $ runMockApp (searchForSong "All Star") mockData
      { mockRecordings = [Recording "All Star"]
      }
  , goldens "searchForSong_GangnamStyle" $ runMockApp (searchForSong "Gangnam Style") mockData
      { mockRecordings = [Recording "Gangnam Style"]
      }
  ]
