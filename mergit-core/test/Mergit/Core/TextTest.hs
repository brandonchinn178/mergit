module Mergit.Core.TextTest where

import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.QuickCheck

import Mergit.Core.Text

test :: TestTree
test =
  testGroup
    "Mergit.Core.Text"
    [ testProperty "fromStagingMessage is inverse of toStagingMessage" $ \(BranchName base) prs ->
        fromStagingMessage (toStagingMessage base prs) === Just (base, prs)
    ]

newtype BranchName = BranchName Text
  deriving (Show)

instance Arbitrary BranchName where
  arbitrary = BranchName . Text.pack <$> listOf1 (elements validChars)
    where
      validChars =
        concat
          [ ['a' .. 'z']
          , ['A' .. 'Z']
          , ['0' .. '9']
          , ['.', '/', '@']
          ]
