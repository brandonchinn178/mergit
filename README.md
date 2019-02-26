# LeapYear Merge Bot

## Quickstart

1. `scripts/install-system-deps.sh`
1. `stack build`
1. `source .env`
1. Go to `smee.io` and start a new channel
1. `smee --url https://smee.io/<token> --path /webhook/`
1. Run configuration steps pointed to `https://smee.io/<token>`
1. `stack exec merge-bot`

## Features and Design Spec

See the Feature home page [on Notion][feature-home-page]

[feature-home-page]: https://www.notion.so/leapyear/Merge-Bot-4c28d412fa7b414fb02e5e3264507b44

## Repo layout

The project is broken up into multiple sub-projects, most of which can be moved
out into a separate repo:

* `graphql`: The `graphql-client` library that exposes an interface for calling
a GraphQL API and consuming the output in a type-safe manner.

* `github-rest`: A library for interacting with the GitHub REST API (v3). More
flexible than the `github` package on Hackage by manually defining endpoints
instead of writing a function per endpoint.

* `servant-github-app`: A library for serving a GitHub App with Servant.
Similar to the `servant-github-webhook` package on Hackage, this library checks
the signature sent with the payload to ensure that the event is sent by GitHub.
This library goes further by following the same steps as the official [GitHub
App template](https://github.com/github-developer/github-app-template) to
provide an access token to use for the duration of a request.

* `merge-bot-core`: The core logic for the merge bot.

* `merge-bot`: The web service that serves the merge bot as a GitHub app.

## Configuration

1. Follow [these instructions][create-github-app] to create a GitHub app.
**Make sure to set a webhook secret!** Save the webhook secret you created and
the App ID and Private key that GitHub generated for you.

1. Set the following environment variables, preferably in a `.env` file:
    1. `GITHUB_APP_ID` to the App ID
    1. `GITHUB_WEBHOOK_SECRET` to the webhook secret you created
    1. `GITHUB_PRIVATE_KEY` to the absolute path of the private key

[create-github-app]: https://developer.github.com/apps/quickstart-guides/setting-up-your-development-environment/#step-2-register-a-new-github-app
