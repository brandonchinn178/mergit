# Merge Bot Architecture

This document will go over how the merge bot project is architected, which may be useful for you if you are either developing on the merge bot or troubleshooting the merge bot.

Broadly speaking, the two projects in this repo to focus on are `merge-bot-core` and `merge-bot`. The other two projects, `github-schemas` and `servant-github-app`, you can think of as third-party libraries. They will be broken out into separate repos at some point.

`merge-bot-core` contains the core functionality of the merge bot. All the business logic for the merge bot (e.g. "merge branch A into `main`") is exposed as a function in this package.

`merge-bot` contains the web application that listens for requests from GitHub, interprets them, and executes the appropriate logic in `merge-bot-core`. The web app also serves HTML debugging pages that shows the current state of the merge bot.

## merge-bot

The entrypoint for this package is `runMergeBot` in `MergeBot.hs` (which is called by the main function in `exe/Main.hs`). This function loads environment variables, initializes some things, and starts the following threads in parallel:
1. The primary thread runs the merge bot web application
1. Another thread listens for merge bot events and executes them
1. Another thread will periodically poll to start the next merge run (TODO: remove the need for polling — [JIRA](https://leapyear.atlassian.net/browse/QA-131))

The merge bot web application is defined in `MergeBot.Routes`, which consists of the following routes:
1. Webhook routes (defined in `MergeBot.Routes.Webhook`), which handles all webhook events sent by GitHub (e.g. "PR was created" or "CI job finished successfully")
1. Debug routes (defined in `MergeBot.Routes.Debug`), which serves HTML pages for debugging
1. Auth routes (defined in `MergeBot.Routes.Auth`), which uses the [GitHub OAuth protocol](https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps) to authenticate a user to be able to access the Debug routes

## merge-bot-core

The public API for this package is found in `MergeBot.Core`. These functions contain the very high-level logic for various merge bot operations. Broadly speaking, if you were to imagine people using the merge bot not via buttons in the GitHub Actions interface but by running Haskell functions, they should only ever need to use the `merge-bot-core` library.

One important submodule is `MergeBot.Core.GitHub`, which contains all the functions that directly hit the GitHub API. Some functions use the [GitHub GraphQL API](https://docs.github.com/en/graphql), some use the [GitHub REST API](https://docs.github.com/en/rest). The GraphQL functions use the GraphQL queries defined in `MergeBot/Core/GraphQL/api/`, which are used to generate `MergeBot.Core.GraphQL.API`. The GraphQL functionality is provided by the [`graphql-client`](https://hackage.haskell.org/package/graphql-client) library. The REST functions use the [`github-rest`](https://hackage.haskell.org/package/github-rest) library.

Another important submodule is `MergeBot.Core.Monad`, which defines the environment (the "monad") that all the functions run in. Specifically, `runBotAppT` is the function that:
* configures the GitHub API settings (e.g. set the auth token, the user agent, and the API version)
* specifies how to handle logging
* specifies how to handle exceptions
