{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MergeBot.Core.GitHubTest where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.GraphQL (MonadGraphQLQuery)
import Data.GraphQL.TestUtils
    (AnyResultMock, MockQueryT, ResultMock(..), mocked, runMockQueryT)
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.GitObjectID (GitObjectID(..))
import GitHub.REST (MonadGitHubREST(..))
import Test.Tasty
import Test.Tasty.QuickCheck

import MergeBot.Core.GitHub
import MergeBot.Core.GraphQL.API (GetCICommitQuery(..))
import MergeBot.Core.Monad (MonadMergeBot(..))
import MergeBot.Core.Text (checkRunTry)

test :: TestTree
test = testGroup "MergeBot.GitHub"
  [ testGetCICommit
  ]

testGetCICommit :: TestTree
testGetCICommit = testGroup "getCICommit"
  [ testProperty "Loads the paginated list of parents" $ \baseBranchSHA (NonEmpty parentSHAs) ->
      forAll (chunks $ baseBranchSHA : parentSHAs) $ \parentSHAsPages ->
        let mocks = mockGetCICommitQueries mockSHA checkRunTry parentSHAsPages
        in ioProperty $ runTestApp mocks $ do
          CICommit{parents} <- getCICommit mockSHA CheckRunTry
          return $ map fst parents === parentSHAs
  ]
  where
    -- Generate a response to a GetCICommitQuery request.
    mkGetCICommitResponse :: [GitObjectID] -> Int -> Int -> Value
    mkGetCICommitResponse parentSHAsInPage pageNum totalPages =
      let hasNextPage = pageNum < totalPages
          endCursor = if hasNextPage then Just $ Text.pack $ show $ pageNum + 1 else Nothing
      in [aesonQQ|
        {
          "repository": {
            "object": {
              "tree": {
                "oid": #{mockSHA},
                "entries": []
              },
              "status": null,
              "parents": {
                "pageInfo": {
                  "hasNextPage": #{hasNextPage},
                  "endCursor": #{endCursor}
                },
                "nodes": #{map mkGetCICommitParentNode parentSHAsInPage}
              }
            }
          }
        }
      |]

    mkGetCICommitParentNode :: GitObjectID -> Value
    mkGetCICommitParentNode parentSHA = [aesonQQ|
      {
        "oid": #{parentSHA},
        "checkSuites": {
          "nodes": [
            {
              "checkRuns": {
                "nodes": [{ "databaseId": 1 }]
              }
            }
          ]
        }
      }
    |]

    mockGetCICommitQueries :: GitObjectID -> Text -> [[GitObjectID]] -> [AnyResultMock]
    mockGetCICommitQueries ciCommitSHA checkName pagedParentSHAs =
      let totalPages = length pagedParentSHAs
          parentSHAsWithPage = zip pagedParentSHAs [1..]

      in flip map parentSHAsWithPage $ \(parentSHAsInPage, pageNum) ->
        mocked ResultMock
          { query = GetCICommitQuery
              { _repoOwner = testRepoOwner
              , _repoName = testRepoName
              , _appId = testAppId
              , _after = if pageNum == 1 then Nothing else Just $ Text.pack $ show pageNum
              , _sha = ciCommitSHA
              , _checkName = Just checkName
              }
          , result = mkGetCICommitResponse parentSHAsInPage pageNum totalPages
          }

{- Mock data -}

testRepoOwner :: Text
testRepoOwner = "LeapYear"

testRepoName :: Text
testRepoName = "my-project"

testAppId :: Int
testAppId = 1

mockSHA :: GitObjectID
mockSHA = GitObjectID $ Text.replicate 40 "0"

{- TestApp helper -}

newtype TestApp a = TestApp { unTestApp :: MockQueryT IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadGraphQLQuery)

runTestApp :: [AnyResultMock] -> TestApp a -> IO a
runTestApp mocks = (`runMockQueryT` mocks) . unTestApp

instance MonadGitHubREST TestApp where
  queryGitHubPage' = error "MonadGitHubREST not implemented for TestApp"

instance MonadMergeBot TestApp where
  getRepo = pure (testRepoOwner, testRepoName)
  getAppId = pure testAppId

instance MonadUnliftIO TestApp where
  -- Note: If you implement this, be sure that only one MockQueryT action is unlifted at a time.
  -- See MonadUnliftIO warnings on implementing it for StateT for more information.
  withRunInIO _ = error "MonadUnliftIO not implemented for TestApp"

{- QuickCheck helpers -}

-- | Arbitrarily chunk the given list.
chunks :: [a] -> Gen [[a]]
chunks [] = return []
chunks xs = do
  n <- choose (1, length xs)
  let (chunk, rest) = splitAt n xs
  (chunk:) <$> chunks rest

{- Orphans -}

instance Arbitrary GitObjectID where
  arbitrary = GitObjectID . Text.pack <$> arbitrarySHA1
    where
      arbitrarySHA1 = vectorOf 40 $ elements "0123456789abcdef"
