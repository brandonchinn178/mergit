{-|
Module      :  MergeBot.Routes.Debug
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines debugging routes for the MergeBot.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MergeBot.Routes.Debug
  ( DebugRoutes
  , handleDebugRoutes
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM, forM_)
import Data.Aeson.Schema (Object, get, schema)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import GitHub.Data.URL (URL(..))
import GitHub.REST (GHEndpoint(..), KeyValue(..), queryGitHub, queryGitHubAll)
import GitHub.Schema.PullRequest (PullRequest)
import GitHub.Schema.Ref (Ref)
import GitHub.Schema.Repository (Repository)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import MergeBot.Auth (xsrfTokenInputName)
import qualified MergeBot.Core.GitHub as Core
import qualified MergeBot.Core.Text as Core
import MergeBot.Monad (MergeBotEvent(..), getInstallations, queueEvent)
import MergeBot.Routes.Debug.Monad
    (DebugApp, ServerDebug, getUser, getXsrfToken, liftBaseApp, runBotAppDebug)

newtype UrlRepo = UrlRepo (Text, Text)

instance ToHttpApiData UrlRepo where
  toUrlPiece (UrlRepo (repoOwner, repoName)) = repoOwner <> "/" <> repoName

instance FromHttpApiData UrlRepo where
  parseUrlPiece urlPiece =
    case Text.splitOn "/" urlPiece of
      [repoOwner, repoName] -> Right $ UrlRepo (repoOwner, repoName)
      _ -> Left $ "Does not match '<repoOwner>/<repoName>': " <> urlPiece

type DebugRoutes =
       IndexPage
  :<|> RepositoryPage
  :<|> DeleteStagingBranch

handleDebugRoutes :: ServerDebug DebugRoutes
handleDebugRoutes =
       handleIndexPage
  :<|> handleRepositoryPage
  :<|> handleDeleteStagingBranch

{- Index page -}

type IndexPage = HtmlPage
handleIndexPage :: DebugApp Html
handleIndexPage = do
  installations <- liftBaseApp getInstallations

  repositories <- fmap concat $ forM installations $ \installationId ->
    [get| .repositories |] <$>
      queryGitHub @_ @(Object [schema| { repositories: List #Repository } |]) GHEndpoint
        { method = GET
        , endpoint = "/user/installations/:installation_id/repositories"
        , endpointVals = [ "installation_id" := installationId ]
        , ghData = []
        }

  render $ do
    H.h2 "Available repositories"
    forM_ repositories $ \repo ->
      let (owner, name) = [get| repo.(owner.login, name) |]
          link = fromLink $ linkTo @RepositoryPage (UrlRepo (owner, name))
      in H.li $ H.a ! A.href link $ H.toHtml [get| repo.full_name |]

{- Repository page -}

type RepositoryPage =
  "repo"
  :> Capture "repo" UrlRepo
  :> HtmlPage

handleRepositoryPage :: UrlRepo -> DebugApp Html
handleRepositoryPage urlRepo@(UrlRepo repo) = do
  let (repoOwner, repoName) = repo

  -- As much as possible, run all GitHub API queries first, to minimize discrepancies
  -- in data changing from underneath us

  allRefs <- queryGitHubAll @_ @[Object Ref] GHEndpoint
    { method = GET
    , endpoint = "/repos/:repoOwner/:repoName/git/refs/heads"
    , endpointVals = [ "repoOwner" := repoOwner, "repoName" := repoName ]
    , ghData = []
    }

  allPRs <- queryGitHubAll @_ @[Object PullRequest] GHEndpoint
    { method = GET
    , endpoint = "/repos/:repoOwner/:repoName/pulls"
    , endpointVals = [ "repoOwner" := repoOwner, "repoName" := repoName ]
    , ghData = []
    }

  queues <- runBotAppDebug repo $ do
    let getPRIds = map $ \(prId, _, _) -> prId
    map (fmap getPRIds) . HashMap.toList <$> Core.getQueues

  -- mergeRuns :: [(BranchName, [PrNum])]
  -- mapping of base branch to list of PR ids running a merge against the base branch
  mergeRuns <- runBotAppDebug repo $
    fmap catMaybes $ forM allRefs $ \ref -> do
      case Text.stripPrefix "refs/heads/" [get| ref.ref |] >>= Core.fromStagingBranch of
        Nothing -> return Nothing
        Just baseBranch -> do
          ciCommit <- Core.getCICommit [get| ref.object.sha |] Core.CheckRunMerge
          prs <- Core.getPRsForCICommit ciCommit
          return $ Just (baseBranch, map Core.prForCommitId prs)

  xsrfToken <- getXsrfToken

  let allPRsMap = HashMap.fromList $ map ([get| .number |] &&& id) allPRs
      lookupPR = (`HashMap.lookup` allPRsMap)

  render $ do
    H.p $ do
      "Viewing: "
      H.strong $ H.toHtml $ repoOwner <> "/" <> repoName
      " ("
      H.a ! A.href "/" $ "Back"
      ")"

    H.h2 "Running pull requests"
    if null mergeRuns
      then H.p "No PRs are running"
      else forM_ mergeRuns $ \(branch, prIds) -> do
        H.h3 $ H.toHtml branch

        -- button to delete staging branch
        let resetStagingPath = fromLink $ linkTo @DeleteStagingBranch urlRepo branch
        H.form ! A.method "post" ! A.action resetStagingPath $ do
          xsrfTokenInput xsrfToken
          H.button "Reset merge run"

        case traverse lookupPR prIds of
          Nothing -> do
            let prs = intercalate ", " $ map (\prId -> "#" ++ show prId) prIds
            mkErrorTable $ Text.pack $ "Found closed PRs (" ++ prs ++ "). Reset this merge run."
          Just prs -> mkTablePRs prs

    H.h2 "Queued pull requests"
    if null queues
      then H.p "No PRs are queued"
      else forM_ queues $ \(branch, prIds) -> do
        H.h3 $ H.toHtml branch

        -- should not error, because a queued PR, by definition, is open
        let idToPR prId = fromMaybe
              (error $ "Could not find open PR #" ++ show prId)
              (lookupPR prId)

        mkTablePRs $ map idToPR prIds

    H.h2 "All open pull requests"
    mkTablePRs allPRs

{- Reset staging branch -}

type DeleteStagingBranch =
  "repo"
  :> Capture "repo" UrlRepo
  :> "reset-merge-run"
  :> Capture "baseBranch" Core.BranchName
  :> Verb 'POST 303 '[HTML] RedirectResponse

handleDeleteStagingBranch :: UrlRepo -> Text -> DebugApp RedirectResponse
handleDeleteStagingBranch urlRepo@(UrlRepo repo) baseBranch = do
  runBotAppDebug repo $
    queueEvent $ DeleteBranch $ Core.toStagingBranch baseBranch

  return $ addHeader (fromLink $ linkTo @RepositoryPage urlRepo) NoContent

{- Helpers -}

type HtmlPage = Get '[HTML] Html
type RedirectResponse = Headers '[Header "Location" String] NoContent

-- | Renders the given body within the general template.
render :: Html -> DebugApp Html
render body = do
  user <- getUser
  return $ H.html $ do
    H.head $
      H.title "LeapYear Merge Bot"
    H.body $ do
      H.header $ do
        H.h1 "LeapYear Merge Bot"
        H.p $ do
          "Logged in as: "
          H.strong $ H.toHtml user
      H.main body

-- | Render a basic table.
mkTable :: [Text] -> [a] -> (a -> Html) -> Html
mkTable headers tableData toCells =
  H.table ! H.customAttribute "border" "1" $ do
    H.tr $ mapM_ (H.th . H.toHtml) headers
    mapM_ (H.tr . toCells) tableData

-- | Render an error in a table.
mkErrorTable :: Text -> Html
mkErrorTable errorMessage = mkTable ["Error", errorMessage] [] id

-- | Render a table of PRs.
mkTablePRs :: [Object PullRequest] -> Html
mkTablePRs prs = mkTable ["#", "title"] prs $ \pr -> do
  H.td $ H.toHtml [get| pr.number |]
  let link = H.toValue $ unURL [get| pr.html_url |]
  H.td $ H.a ! A.href link $ H.toHtml [get| pr.title |]

linkTo :: forall endpoint. (IsElem endpoint DebugRoutes, HasLink endpoint) => MkLink endpoint Link
linkTo = safeLink (Proxy @DebugRoutes) (Proxy @endpoint)

fromLink :: IsString s => Link -> s
fromLink = fromString . ('/':) . show . linkURI

xsrfTokenInput :: Text -> Html
xsrfTokenInput xsrfToken =
  H.input
    ! A.type_ "hidden"
    ! A.name (H.toValue xsrfTokenInputName)
    ! A.value (H.toValue xsrfToken)
