# Mergit Architecture

This document will go over how the Mergit project is architected, which may be useful for you if you are either developing on or troubleshooting Mergit.

Broadly speaking, the two projects in this repo to focus on are `mergit-core` and `mergit`. The other two projects, `github-schemas` and `servant-github-app`, you can think of as third-party libraries. They will be broken out into separate repos at some point.

`mergit-core` contains the core functionality of Mergit. All the business logic for Mergit (e.g. "merge branch A into `main`") is exposed as a function in this package.

`mergit` contains the web application that listens for requests from GitHub, interprets them, and executes the appropriate logic in `mergit-core`. The web app also serves HTML debugging pages that shows the current state of Mergit.

## mergit

The entrypoint for this package is `runMergit` in `Mergit.hs` (which is called by the main function in `exe/Main.hs`). This function loads environment variables, initializes some things, and starts the following threads in parallel:
1. The primary thread runs the Mergit web application
1. Another thread listens for Mergit events and executes them
1. Another thread will periodically poll to start the next merge run (TODO: remove the need for polling — [JIRA](https://leapyear.atlassian.net/browse/QA-131))

The Mergit web application is defined in `Mergit.Routes`, which consists of the following routes:
1. Webhook routes (defined in `Mergit.Routes.Webhook`), which handles all webhook events sent by GitHub (e.g. "PR was created" or "CI job finished successfully")
1. Debug routes (defined in `Mergit.Routes.Debug`), which serves HTML pages for debugging
1. Auth routes (defined in `Mergit.Routes.Auth`), which uses the [GitHub OAuth protocol](https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps) to authenticate a user to be able to access the Debug routes

## mergit-core

The public API for this package is found in `Mergit.Core`. These functions contain the very high-level logic for various Mergit operations. Broadly speaking, if you were to imagine people using Mergit not via buttons in the GitHub Actions interface but by running Haskell functions, they should only ever need to use the `mergit-core` library.

One important submodule is `Mergit.Core.GitHub`, which contains all the functions that directly hit the GitHub API. Some functions use the [GitHub GraphQL API](https://docs.github.com/en/graphql), some use the [GitHub REST API](https://docs.github.com/en/rest). The GraphQL functions use the GraphQL queries defined in `Mergit/Core/GraphQL/api/`, which are used to generate `Mergit.Core.GraphQL.API`. The GraphQL functionality is provided by the [`graphql-client`](https://hackage.haskell.org/package/graphql-client) library. The REST functions use the [`github-rest`](https://hackage.haskell.org/package/github-rest) library.

Another important submodule is `Mergit.Core.Monad`, which defines the environment (the "monad") that all the functions run in. Specifically, `runBotAppT` is the function that:
* configures the GitHub API settings (e.g. set the auth token, the user agent, and the API version)
* specifies how to handle logging
* specifies how to handle exceptions
