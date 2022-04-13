{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mergit.Core.ErrorTest where

import Data.Text (Text)
import Data.Text qualified as Text
import GitHub.Data.GitObjectID (GitObjectID (..))
import Test.Tasty
import Test.Tasty.QuickCheck

import Mergit.Core.Error

test :: TestTree
test =
  testGroup
    "Mergit.Core.Error"
    [ testProperty "All errors can be converted into messages" (total . getMergitError)
    ]

{- ORMOLU_DISABLE -}
{- https://github.com/fourmolu/fourmolu/issues/72-}

instance Arbitrary MergitError where
  -- for now, not generating any errors containing 'Object schema'
  arbitrary =
    oneof
      [ AmbiguousPRForCommit <$> arbitrary
      , BadUpdate <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      -- , CannotDetermineCheckRunPR <$> arbitrary
      -- , CIBranchPushed <$> arbitrary
      , CICommitMissingParents <$> arbitrary <*> arbitrary <*> arbitrary
      , CommitLacksPR <$> arbitrary
      , ConfigFileInvalid <$> arbitrary <*> arbitrary
      , ConfigFileMissing <$> arbitrary
      , InvalidStaging <$> arbitrary <*> arbitrary
      , MergeConflict <$> arbitrary
      , MissingBaseBranch <$> arbitrary <*> arbitrary
      , MissingCheckRun <$> arbitrary <*> arbitrary
      , MissingCheckRunPR <$> arbitrary <*> arbitrary
      , PRWasUpdatedDuringMergeRun <$> arbitrary <*> arbitrary <*> arbitrary
      , SomePRsMerged <$> arbitrary <*> arbitrary
      , TreeNotUpdated <$> arbitrary <*> arbitrary
      , UnapprovedPR <$> arbitrary
      ]
    where
      -- This will fail if someone adds a constructor. If you're adding a constructor,
      -- make sure to add it above.
      _ = \case
        AmbiguousPRForCommit{} -> ()
        BadUpdate{} -> ()
        CannotDetermineCheckRunPR{} -> ()
        CIBranchPushed{} -> ()
        CICommitMissingParents{} -> ()
        CommitLacksPR{} -> ()
        ConfigFileInvalid{} -> ()
        ConfigFileMissing{} -> ()
        InvalidStaging{} -> ()
        MergeConflict{} -> ()
        MissingBaseBranch{} -> ()
        MissingCheckRun{} -> ()
        MissingCheckRunPR{} -> ()
        PRWasUpdatedDuringMergeRun{} -> ()
        SomePRsMerged{} -> ()
        TreeNotUpdated{} -> ()
        UnapprovedPR{} -> ()

{- ORMOLU_ENABLE -}

deriving instance Arbitrary GitObjectID

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
