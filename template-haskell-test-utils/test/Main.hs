{-# LANGUAGE TemplateHaskell #-}

import Data.Void (Void)
import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.TH.TestUtils
import TH

main :: IO ()
main = defaultMain $ testGroup "template-haskell-test-utils"
  [ testCase "Maybe" $
    $(tryQ' $ firstConstrForType "Maybe") @== Right "Nothing"
  , testCase "NonExistent" $
    $(tryQ' $ firstConstrForType "NonExistent") @== Left "Type does not exist: NonExistent"
  , testCase "Show" $
    $(tryQ' $ firstConstrForType "Show") @== Left "Not a data type: Show"
  , testCase "Void" $
    $(tryQ' $ firstConstrForType "Void") @== Left "Data type has no constructors: Void"
  ]

-- | Helper to specify the type of the splice.
(@==) :: Either String String -> Either String String -> IO ()
(@==) = (@?=)
