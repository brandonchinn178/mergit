{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MergeBot.Core.GitHubTest where

import Control.Monad ((>=>))
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
import UnliftIO.Exception (try)

import MergeBot.Core.Error (BotError(..))
import MergeBot.Core.GitHub
import MergeBot.Core.GraphQL.API (GetCICommitQuery(..), GetPRForCommitQuery(..))
import MergeBot.Core.Monad (MonadMergeBot(..))
import MergeBot.Core.Text (checkRunTry)

test :: TestTree
test = testGroup "MergeBot.Core.GitHub"
  [ testGetCICommit
  , testGetPRForCommit
  ]

testGetCICommit :: TestTree
testGetCICommit = testGroup "getCICommit"
  [ testProperty "Loads the paginated list of parents" $ \baseBranchSHA (NonEmpty parentSHAs) ->
      let ciCommitParents = (baseBranchSHA, []) : map (, [1]) parentSHAs
      in forAll (chunks ciCommitParents) $ \pagedParents ->
        let mocks = mockGetCICommitQueries mockSHA checkRunTry pagedParents
        in ioProperty $ runTestApp mocks $ do
          CICommit{parents} <- getCICommit mockSHA CheckRunTry
          return $ map fst parents === parentSHAs
  ]
  where
    -- Generate a response to a GetCICommitQuery request.
    mkGetCICommitResponse :: [(GitObjectID, [CheckRunId])] -> Int -> Int -> Value
    mkGetCICommitResponse parentsInPage pageNum totalPages =
      let hasNextPage = pageNum < totalPages
          endCursor = if hasNextPage then Just $ Text.pack $ show $ pageNum + 1 else Nothing
      in [aesonQQ|
        {
          "repository": {
            "object": {
              "message": "Fake message",
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
                "nodes": #{map mkGetCICommitParentNode parentsInPage}
              }
            }
          }
        }
      |]

    mkGetCICommitParentNode :: (GitObjectID, [CheckRunId]) -> Value
    mkGetCICommitParentNode (parentSHA, checkRunIds) = [aesonQQ|
      {
        "oid": #{parentSHA},
        "checkSuites": {
          "nodes": [
            {
              "checkRuns": {
                "nodes": #{map mkGetCICommitCheckRunNode checkRunIds}
              }
            }
          ]
        }
      }
    |]

    mkGetCICommitCheckRunNode :: CheckRunId -> Value
    mkGetCICommitCheckRunNode checkRunId = [aesonQQ| { "databaseId": #{checkRunId} } |]

    mockGetCICommitQueries :: GitObjectID -> Text -> [[(GitObjectID, [CheckRunId])]] -> [AnyResultMock]
    mockGetCICommitQueries ciCommitSHA checkName pagedParents =
      let totalPages = length pagedParents
          parentsWithPageNum = zip pagedParents [1..]

      in flip map parentsWithPageNum $ \(parentsInPage, pageNum) ->
        mocked ResultMock
          { query = GetCICommitQuery
              { _repoOwner = testRepoOwner
              , _repoName = testRepoName
              , _appId = testAppId
              , _after = if pageNum == 1 then Nothing else Just $ Text.pack $ show pageNum
              , _sha = ciCommitSHA
              , _checkName = Just checkName
              }
          , result = mkGetCICommitResponse parentsInPage pageNum totalPages
          }

testGetPRForCommit :: TestTree
testGetPRForCommit = testGroup "getPRForCommit"
  [ testProperty "Returns single associated pull request" $ \sha pr -> ioProperty $ do
      let mocks = mockGetPRForCommitQueries sha [[pr]]
      result <- runTestApp mocks $ getPRForCommit sha
      return $ result === pr
  , testProperty "Returns pull request with matching SHA" $ \sha pr' ->
      let pr = pr' { prSHA = sha } in
      forAll (listOf $ prNotMatching sha) $ \otherPRs ->
        forAll (shuffledChunks $ pr:otherPRs) $ \pagedPRs ->
          ioProperty $ do
            let mocks = mockGetPRForCommitQueries sha pagedPRs
            result <- runTestApp mocks $ getPRForCommit sha
            return $ result === pr
  , testProperty "Errors with no associated pull requests" $ \sha -> ioProperty $ do
      let mocks = mockGetPRForCommitQueries sha [[]]
      result <- try $ runTestApp mocks $ getPRForCommit sha
      return $ result === Left (CommitLacksPR sha)
  , testProperty "Errors with multiple non-matching associated pull requests" $ \sha -> do
      forAll (listOfAtLeast 2 $ prNotMatching sha) $ \prs ->
        forAll (shuffledChunks prs) $ \pagedPRs ->
          ioProperty $ do
            let mocks = mockGetPRForCommitQueries sha pagedPRs
            result <- try $ runTestApp mocks $ getPRForCommit sha
            return $ result === Left (AmbiguousPRForCommit sha)
  ]
  where
    prNotMatching sha = arbitrary `suchThat` ((/= sha) . prSHA)

    mockGetPRForCommitQueries sha = withPaged $ \Page{..} ->
      mocked ResultMock
        { query = GetPRForCommitQuery
            { _repoOwner = testRepoOwner
            , _repoName = testRepoName
            , _sha = sha
            , _after = pageOffset
            }
        , result =
            [aesonQQ|
              {
                "repository": {
                  "object": {
                    "associatedPullRequests": {
                      "pageInfo": {
                        "hasNextPage": #{pageHasNext},
                        "endCursor": #{pageNextNum}
                      },
                      "nodes": #{map fromPR pageData}
                    }
                  }
                }
              }
            |]
        }

    fromPR PullRequest{..} =
      [aesonQQ|
        {
          "number": #{prId},
          "baseRefName": #{prBaseBranch},
          "headRefOid": #{prSHA},
          "headRefName": #{prBranch},
          "merged": #{prIsMerged}
        }
      |]

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

{- Pagination helpers -}

data Page a = Page
  { pageData    :: [a]
  , pageHasNext :: Bool
  , pageNextNum :: Maybe Text
  , pageOffset  :: Maybe Text
  }

withPaged :: (Page a -> b) -> [[a]] -> [b]
withPaged f pages = zipWith (curry (f . mkPage)) pages [1..]
  where
    totalPages = length pages
    mkPage (pageData, pageNum) =
      let pageHasNext = pageNum < totalPages
          pageNextNum =
            if pageHasNext
              then Just $ Text.pack $ show $ pageNum + 1
              else Nothing
          pageOffset =
            if pageNum == 1
              then Nothing
              else Just $ Text.pack $ show pageNum
      in Page{..}

{- QuickCheck helpers -}

-- | Arbitrarily chunk the given list.
chunks :: [a] -> Gen [[a]]
chunks [] = return []
chunks xs = do
  n <- choose (1, length xs)
  let (chunk, rest) = splitAt n xs
  (chunk:) <$> chunks rest

listOfAtLeast :: Int -> Gen a -> Gen [a]
listOfAtLeast n gen = (++) <$> vectorOf n gen <*> listOf gen

-- | Shuffle the given list and arbitrarily break it up into chunks.
shuffledChunks :: [a] -> Gen [[a]]
shuffledChunks = shuffle >=> chunks

{- Orphans -}

instance Arbitrary GitObjectID where
  arbitrary = GitObjectID . Text.pack <$> arbitrarySHA1
    where
      arbitrarySHA1 = vectorOf 40 $ elements "0123456789abcdef"

instance Arbitrary PullRequest where
  arbitrary = PullRequest
    <$> arbitrary
    <*> (Text.pack . getPrintableString <$> arbitrary)
    <*> arbitrary
    <*> (Text.pack . getPrintableString <$> arbitrary)
    <*> arbitrary
