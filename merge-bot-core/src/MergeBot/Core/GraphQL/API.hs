{- This file was automatically generated and should not be edited. -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module MergeBot.Core.GraphQL.API where

import Data.GraphQL
import Data.GraphQL.Bootstrap

import MergeBot.Core.GraphQL.Scalars
import MergeBot.Core.GraphQL.Enums.PullRequestReviewState
import MergeBot.Core.GraphQL.Enums.StatusState

{-----------------------------------------------------------------------------
* getBranchSHA

-- result :: Object GetBranchSHASchema; throws a GraphQL exception on errors
result <- runQuery GetBranchSHAQuery
  { _repoOwner = ...
  , _repoName = ...
  , _branch = ...
  }

-- result :: GraphQLResult (Object GetBranchSHASchema)
result <- runQuerySafe GetBranchSHAQuery
  { _repoOwner = ...
  , _repoName = ...
  , _branch = ...
  }
-----------------------------------------------------------------------------}

data GetBranchSHAQuery = GetBranchSHAQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _branch :: Text
  }
  deriving (Show)

type GetBranchSHASchema = [schema|
  {
    repository: Maybe {
      ref: Maybe {
        target: Maybe {
          oid: GitObjectID,
        },
      },
    },
  }
|]

instance GraphQLQuery GetBranchSHAQuery where
  type ResultSchema GetBranchSHAQuery = GetBranchSHASchema

  getQueryName _ = "getBranchSHA"

  getQueryText _ = [query|
    query getBranchSHA($repoOwner: String!, $repoName: String!, $branch: String!) {
      repository(owner: $repoOwner, name: $repoName) {
        ref(qualifiedName: $branch) {
          target {
            oid
          }
        }
      }
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetBranchSHAQuery)
    , "repoName" .= _repoName (query :: GetBranchSHAQuery)
    , "branch" .= _branch (query :: GetBranchSHAQuery)
    ]

{-----------------------------------------------------------------------------
* getBranchTree

-- result :: Object GetBranchTreeSchema; throws a GraphQL exception on errors
result <- runQuery GetBranchTreeQuery
  { _repoOwner = ...
  , _repoName = ...
  , _name = ...
  }

-- result :: GraphQLResult (Object GetBranchTreeSchema)
result <- runQuerySafe GetBranchTreeQuery
  { _repoOwner = ...
  , _repoName = ...
  , _name = ...
  }
-----------------------------------------------------------------------------}

data GetBranchTreeQuery = GetBranchTreeQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _name :: Text
  }
  deriving (Show)

type GetBranchTreeSchema = [schema|
  {
    repository: Maybe {
      ref: Maybe {
        target: Maybe {
          [__fragment]: Try (
            {
              tree: {
                oid: GitObjectID,
                entries: Maybe List {
                  name: Text,
                  object: Maybe {
                    [__fragment]: Try (
                      {
                        text: Maybe Text,
                      }
                    ),
                  },
                },
              },
            }
          ),
        },
      },
    },
  }
|]

instance GraphQLQuery GetBranchTreeQuery where
  type ResultSchema GetBranchTreeQuery = GetBranchTreeSchema

  getQueryName _ = "getBranchTree"

  getQueryText _ = [query|
    query getBranchTree($repoOwner: String!, $repoName: String!, $name: String!) {
      repository(owner: $repoOwner, name: $repoName) {
        ref(qualifiedName: $name) {
          target {
            ... on Commit {
              tree {
                oid
                entries {
                  name
                  object {
                    ... on Blob {
                      text
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetBranchTreeQuery)
    , "repoName" .= _repoName (query :: GetBranchTreeQuery)
    , "name" .= _name (query :: GetBranchTreeQuery)
    ]

{-----------------------------------------------------------------------------
* getCICommit

-- result :: Object GetCICommitSchema; throws a GraphQL exception on errors
result <- runQuery GetCICommitQuery
  { _repoOwner = ...
  , _repoName = ...
  , _sha = ...
  , _after = ...
  , _appId = ...
  , _checkName = ...
  }

-- result :: GraphQLResult (Object GetCICommitSchema)
result <- runQuerySafe GetCICommitQuery
  { _repoOwner = ...
  , _repoName = ...
  , _sha = ...
  , _after = ...
  , _appId = ...
  , _checkName = ...
  }
-----------------------------------------------------------------------------}

data GetCICommitQuery = GetCICommitQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _sha :: GitObjectID
  , _after :: Maybe Text
  , _appId :: Int
  , _checkName :: Maybe Text
  }
  deriving (Show)

type GetCICommitSchema = [schema|
  {
    repository: Maybe {
      object: Maybe {
        [__fragment]: Try (
          {
            message: Text,
            tree: {
              oid: GitObjectID,
              entries: Maybe List {
                name: Text,
                object: Maybe {
                  [__fragment]: Try (
                    {
                      text: Maybe Text,
                    }
                  ),
                },
              },
            },
            status: Maybe {
              contexts: List {
                context: Text,
                state: StatusState,
                targetUrl: Maybe URI,
              },
            },
            parents: {
              pageInfo: {
                hasNextPage: Bool,
                endCursor: Maybe Text,
              },
              nodes: Maybe List Maybe {
                oid: GitObjectID,
                checkSuites: Maybe {
                  nodes: Maybe List Maybe {
                    checkRuns: Maybe {
                      nodes: Maybe List Maybe {
                        databaseId: Maybe Int,
                      },
                    },
                  },
                },
              },
            },
          }
        ),
      },
    },
  }
|]

instance GraphQLQuery GetCICommitQuery where
  type ResultSchema GetCICommitQuery = GetCICommitSchema

  getQueryName _ = "getCICommit"

  getQueryText _ = [query|
    query getCICommit($repoOwner: String!, $repoName: String!, $sha: GitObjectID!, $after: String, $appId: Int!, $checkName: String) {
      repository(owner: $repoOwner, name: $repoName) {
        object(oid: $sha) {
          ... on Commit {
            message
            tree {
              oid
              entries {
                name
                object {
                  ... on Blob {
                    text
                  }
                }
              }
            }
            status {
              contexts {
                context
                state
                targetUrl
              }
            }
            parents(first: 5, after: $after) {
              pageInfo {
                hasNextPage
                endCursor
              }
              nodes {
                oid
                ...CommitCheckRunFragment
              }
            }
          }
        }
      }
    }
    fragment CommitCheckRunFragment on Commit {
      checkSuites(filterBy: {appId: $appId}, first: 1) {
        nodes {
          checkRuns(filterBy: {checkName: $checkName}, first: 2) {
            nodes {
              ...CheckRunInfoFragment
            }
          }
        }
      }
    }
    fragment CheckRunInfoFragment on CheckRun {
      databaseId
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetCICommitQuery)
    , "repoName" .= _repoName (query :: GetCICommitQuery)
    , "sha" .= _sha (query :: GetCICommitQuery)
    , "after" .= _after (query :: GetCICommitQuery)
    , "appId" .= _appId (query :: GetCICommitQuery)
    , "checkName" .= _checkName (query :: GetCICommitQuery)
    ]

{-----------------------------------------------------------------------------
* getIsPRMerged

-- result :: Object GetIsPRMergedSchema; throws a GraphQL exception on errors
result <- runQuery GetIsPRMergedQuery
  { _repoOwner = ...
  , _repoName = ...
  , _prNum = ...
  }

-- result :: GraphQLResult (Object GetIsPRMergedSchema)
result <- runQuerySafe GetIsPRMergedQuery
  { _repoOwner = ...
  , _repoName = ...
  , _prNum = ...
  }
-----------------------------------------------------------------------------}

data GetIsPRMergedQuery = GetIsPRMergedQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _prNum :: Int
  }
  deriving (Show)

type GetIsPRMergedSchema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        merged: Bool,
      },
    },
  }
|]

instance GraphQLQuery GetIsPRMergedQuery where
  type ResultSchema GetIsPRMergedQuery = GetIsPRMergedSchema

  getQueryName _ = "getIsPRMerged"

  getQueryText _ = [query|
    query getIsPRMerged($repoOwner: String!, $repoName: String!, $prNum: Int!) {
      repository(owner: $repoOwner, name: $repoName) {
        pullRequest(number: $prNum) {
          merged
        }
      }
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetIsPRMergedQuery)
    , "repoName" .= _repoName (query :: GetIsPRMergedQuery)
    , "prNum" .= _prNum (query :: GetIsPRMergedQuery)
    ]

{-----------------------------------------------------------------------------
* getPRById

-- result :: Object GetPRByIdSchema; throws a GraphQL exception on errors
result <- runQuery GetPRByIdQuery
  { _repoOwner = ...
  , _repoName = ...
  , _id = ...
  }

-- result :: GraphQLResult (Object GetPRByIdSchema)
result <- runQuerySafe GetPRByIdQuery
  { _repoOwner = ...
  , _repoName = ...
  , _id = ...
  }
-----------------------------------------------------------------------------}

data GetPRByIdQuery = GetPRByIdQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _id :: Int
  }
  deriving (Show)

type GetPRByIdSchema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        number: Int,
        baseRefName: Text,
        headRefOid: GitObjectID,
        headRefName: Text,
        merged: Bool,
      },
    },
  }
|]

instance GraphQLQuery GetPRByIdQuery where
  type ResultSchema GetPRByIdQuery = GetPRByIdSchema

  getQueryName _ = "getPRById"

  getQueryText _ = [query|
    query getPRById($repoOwner: String!, $repoName: String!, $id: Int!) {
      repository(owner: $repoOwner, name: $repoName) {
        pullRequest(number: $id) {
          number
          baseRefName
          headRefOid
          headRefName
          merged
        }
      }
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetPRByIdQuery)
    , "repoName" .= _repoName (query :: GetPRByIdQuery)
    , "id" .= _id (query :: GetPRByIdQuery)
    ]

{-----------------------------------------------------------------------------
* getPRCheckRun

-- result :: Object GetPRCheckRunSchema; throws a GraphQL exception on errors
result <- runQuery GetPRCheckRunQuery
  { _repoOwner = ...
  , _repoName = ...
  , _prNum = ...
  , _appId = ...
  , _checkName = ...
  }

-- result :: GraphQLResult (Object GetPRCheckRunSchema)
result <- runQuerySafe GetPRCheckRunQuery
  { _repoOwner = ...
  , _repoName = ...
  , _prNum = ...
  , _appId = ...
  , _checkName = ...
  }
-----------------------------------------------------------------------------}

data GetPRCheckRunQuery = GetPRCheckRunQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _prNum :: Int
  , _appId :: Int
  , _checkName :: Text
  }
  deriving (Show)

type GetPRCheckRunSchema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        commits: {
          nodes: Maybe List Maybe {
            commit: {
              checkSuites: Maybe {
                nodes: Maybe List Maybe {
                  checkRuns: Maybe {
                    nodes: Maybe List Maybe {
                      databaseId: Maybe Int,
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  }
|]

instance GraphQLQuery GetPRCheckRunQuery where
  type ResultSchema GetPRCheckRunQuery = GetPRCheckRunSchema

  getQueryName _ = "getPRCheckRun"

  getQueryText _ = [query|
    query getPRCheckRun($repoOwner: String!, $repoName: String!, $prNum: Int!, $appId: Int!, $checkName: String!) {
      repository(owner: $repoOwner, name: $repoName) {
        pullRequest(number: $prNum) {
          commits(last: 1) {
            nodes {
              commit {
                ...CommitCheckRunFragment
              }
            }
          }
        }
      }
    }
    fragment CommitCheckRunFragment on Commit {
      checkSuites(filterBy: {appId: $appId}, first: 1) {
        nodes {
          checkRuns(filterBy: {checkName: $checkName}, first: 2) {
            nodes {
              ...CheckRunInfoFragment
            }
          }
        }
      }
    }
    fragment CheckRunInfoFragment on CheckRun {
      databaseId
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetPRCheckRunQuery)
    , "repoName" .= _repoName (query :: GetPRCheckRunQuery)
    , "prNum" .= _prNum (query :: GetPRCheckRunQuery)
    , "appId" .= _appId (query :: GetPRCheckRunQuery)
    , "checkName" .= _checkName (query :: GetPRCheckRunQuery)
    ]

{-----------------------------------------------------------------------------
* getPRForCommit

-- result :: Object GetPRForCommitSchema; throws a GraphQL exception on errors
result <- runQuery GetPRForCommitQuery
  { _repoOwner = ...
  , _repoName = ...
  , _sha = ...
  , _after = ...
  }

-- result :: GraphQLResult (Object GetPRForCommitSchema)
result <- runQuerySafe GetPRForCommitQuery
  { _repoOwner = ...
  , _repoName = ...
  , _sha = ...
  , _after = ...
  }
-----------------------------------------------------------------------------}

data GetPRForCommitQuery = GetPRForCommitQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _sha :: GitObjectID
  , _after :: Maybe Text
  }
  deriving (Show)

type GetPRForCommitSchema = [schema|
  {
    repository: Maybe {
      object: Maybe {
        [__fragment]: Try (
          {
            associatedPullRequests: Maybe {
              pageInfo: {
                hasNextPage: Bool,
                endCursor: Maybe Text,
              },
              nodes: Maybe List Maybe {
                number: Int,
                baseRefName: Text,
                headRefOid: GitObjectID,
                headRefName: Text,
                merged: Bool,
              },
            },
          }
        ),
      },
    },
  }
|]

instance GraphQLQuery GetPRForCommitQuery where
  type ResultSchema GetPRForCommitQuery = GetPRForCommitSchema

  getQueryName _ = "getPRForCommit"

  getQueryText _ = [query|
    query getPRForCommit($repoOwner: String!, $repoName: String!, $sha: GitObjectID!, $after: String) {
      repository(owner: $repoOwner, name: $repoName) {
        object(oid: $sha) {
          ... on Commit {
            associatedPullRequests(first: 10, after: $after) {
              pageInfo {
                hasNextPage
                endCursor
              }
              nodes {
                number
                baseRefName
                headRefOid
                headRefName
                merged
              }
            }
          }
        }
      }
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetPRForCommitQuery)
    , "repoName" .= _repoName (query :: GetPRForCommitQuery)
    , "sha" .= _sha (query :: GetPRForCommitQuery)
    , "after" .= _after (query :: GetPRForCommitQuery)
    ]

{-----------------------------------------------------------------------------
* getPRReviews

-- result :: Object GetPRReviewsSchema; throws a GraphQL exception on errors
result <- runQuery GetPRReviewsQuery
  { _repoOwner = ...
  , _repoName = ...
  , _prNum = ...
  , _after = ...
  }

-- result :: GraphQLResult (Object GetPRReviewsSchema)
result <- runQuerySafe GetPRReviewsQuery
  { _repoOwner = ...
  , _repoName = ...
  , _prNum = ...
  , _after = ...
  }
-----------------------------------------------------------------------------}

data GetPRReviewsQuery = GetPRReviewsQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _prNum :: Int
  , _after :: Maybe Text
  }
  deriving (Show)

type GetPRReviewsSchema = [schema|
  {
    repository: Maybe {
      pullRequest: Maybe {
        reviews: Maybe {
          pageInfo: {
            hasNextPage: Bool,
            endCursor: Maybe Text,
          },
          nodes: Maybe List Maybe {
            author: Maybe {
              login: Text,
            },
            state: PullRequestReviewState,
          },
        },
      },
    },
  }
|]

instance GraphQLQuery GetPRReviewsQuery where
  type ResultSchema GetPRReviewsQuery = GetPRReviewsSchema

  getQueryName _ = "getPRReviews"

  getQueryText _ = [query|
    query getPRReviews($repoOwner: String!, $repoName: String!, $prNum: Int!, $after: String) {
      repository(owner: $repoOwner, name: $repoName) {
        pullRequest(number: $prNum) {
          reviews(first: 10, after: $after) {
            pageInfo {
              hasNextPage
              endCursor
            }
            nodes {
              author {
                login
              }
              state
            }
          }
        }
      }
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetPRReviewsQuery)
    , "repoName" .= _repoName (query :: GetPRReviewsQuery)
    , "prNum" .= _prNum (query :: GetPRReviewsQuery)
    , "after" .= _after (query :: GetPRReviewsQuery)
    ]

{-----------------------------------------------------------------------------
* getQueuedPRs

-- result :: Object GetQueuedPRsSchema; throws a GraphQL exception on errors
result <- runQuery GetQueuedPRsQuery
  { _repoOwner = ...
  , _repoName = ...
  , _after = ...
  , _appId = ...
  , _checkName = ...
  }

-- result :: GraphQLResult (Object GetQueuedPRsSchema)
result <- runQuerySafe GetQueuedPRsQuery
  { _repoOwner = ...
  , _repoName = ...
  , _after = ...
  , _appId = ...
  , _checkName = ...
  }
-----------------------------------------------------------------------------}

data GetQueuedPRsQuery = GetQueuedPRsQuery
  { _repoOwner :: Text
  , _repoName :: Text
  , _after :: Maybe Text
  , _appId :: Int
  , _checkName :: Text
  }
  deriving (Show)

type GetQueuedPRsSchema = [schema|
  {
    repository: Maybe {
      pullRequests: {
        pageInfo: {
          hasNextPage: Bool,
          endCursor: Maybe Text,
        },
        nodes: Maybe List Maybe {
          number: Int,
          baseRefName: Text,
          headRefOid: GitObjectID,
          headRef: Maybe {
            target: Maybe {
              [__fragment]: Try (
                {
                  checkSuites: Maybe {
                    nodes: Maybe List Maybe {
                      checkRuns: Maybe {
                        nodes: Maybe List Maybe {
                          databaseId: Maybe Int,
                        },
                      },
                    },
                  },
                }
              ),
            },
          },
        },
      },
    },
  }
|]

instance GraphQLQuery GetQueuedPRsQuery where
  type ResultSchema GetQueuedPRsQuery = GetQueuedPRsSchema

  getQueryName _ = "getQueuedPRs"

  getQueryText _ = [query|
    query getQueuedPRs($repoOwner: String!, $repoName: String!, $after: String, $appId: Int!, $checkName: String!) {
      repository(owner: $repoOwner, name: $repoName) {
        pullRequests(first: 100, after: $after, states: OPEN) {
          pageInfo {
            hasNextPage
            endCursor
          }
          nodes {
            number
            baseRefName
            headRefOid
            headRef {
              target {
                ... on Commit {
                  checkSuites(filterBy: {appId: $appId}, first: 1) {
                    nodes {
                      checkRuns(filterBy: {checkName: $checkName, status: QUEUED}, first: 1) {
                        nodes {
                          ...CheckRunInfoFragment
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    fragment CheckRunInfoFragment on CheckRun {
      databaseId
    }
  |]

  getArgs query = object
    [ "repoOwner" .= _repoOwner (query :: GetQueuedPRsQuery)
    , "repoName" .= _repoName (query :: GetQueuedPRsQuery)
    , "after" .= _after (query :: GetQueuedPRsQuery)
    , "appId" .= _appId (query :: GetQueuedPRsQuery)
    , "checkName" .= _checkName (query :: GetQueuedPRsQuery)
    ]

