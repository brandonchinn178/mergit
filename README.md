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

The merge bot includes the following features:

* Run CI for PRs as if they were merged with the base branch, catching semantic
  merge conflicts, before merging them
* Run CI for PRs on command for testing, to not waste CI cycles on spurious
  pushes

See the Feature home page [on Notion][feature-home-page] for more details.

[feature-home-page]: https://www.notion.so/leapyear/Merge-Bot-4c28d412fa7b414fb02e5e3264507b44

## Repo layout

The project is broken up into multiple sub-projects, most of which can be moved
out into a separate repo:

* `aeson-schemas`: A library that extracts information from JSON input using
type-level schemas and quasiquoters, consuming JSON data in a type-safe manner.
Better than `aeson` for decoding nested JSON data that would be cumbersome to
represent as Haskell ADTs.

* `template-haskell-test-utils`: A library that defines utilities for testing
Template Haskell code.

* `graphql`: The `graphql-client` library that exposes an interface for calling
a GraphQL API and consuming the output using `aeson-schemas`.

* `github-rest`: A library for interacting with the GitHub REST API (v3). More
flexible than the `github` package on Hackage by manually defining endpoints
instead of writing a function per endpoint.

* `github-schemas`: A library containing schemas (defined with `aeson-schemas`)
and data types for interacting with GitHub APIs.

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
    1. **Make sure to set a webhook secret!**
    1. Permissions:
        * Checks: Read & Write
        * Repository contents: Read & Write
        * Repository metadata: Read-only
        * Pull requests: Read & Write
        * Commit statuses: Read-only
    1. Events:
        * Check run
        * Check suite
        * Pull request
        * Push
        * Status
    1. Save the webhook secret you created and the App ID and Private key that
       GitHub generated for you.

1. Set the following environment variables, preferably in a `.env` file based
   on `.env.template`:
    1. `GITHUB_APP_ID` to the App ID
    1. `GITHUB_WEBHOOK_SECRET` to the webhook secret you created
    1. `GITHUB_PRIVATE_KEY` to the absolute path of the private key
    1. `GITHUB_USER_AGENT` to the [user agent][user-agent] to use with the API

[create-github-app]: https://developer.github.com/apps/quickstart-guides/setting-up-your-development-environment/#step-2-register-a-new-github-app
[user-agent]: https://developer.github.com/v3/#user-agent-required

## Deployment

To deploy, follow the instructions in the "Configuration" section to create a
GitHub app with the following parameters:

* Name: LeapYear Merge Bot
* Homepage URL: Set to a `smee.io` URL for now
* Webhook URL: Same as Homepage URL
* Webhook secret: Generate a random secret
* Installed only on this account

Then save the webhook secret, app ID, and private key to LastPass. Follow the
instructions in `deploy/README.md` with these values.

Edit the GitHub app and replace the homepage and webhook URLs with
`https://merge-bot.build-leapyear.com/webhook`. The GitHub app should now be
set!

### Installation

Now, you need to install the GitHub app to the relevant repositories:

1. Remove all CI statuses from required status checks (for protected branches)
1. Add `.lymerge.yaml` to the repo with the required CI statuses (the same
   labels that you removed in step 1):

```yaml
statuses:
- "ci/circleci: build"
- "ci/circleci: test"
```

1. Edit CI config to only run CI on branches matching `staging-*` or `trying-*`
1. Make a PR
1. Set the "Bot Merge" check as a required status check (for protected branches)
1. Use the merge bot to merge in the PR!
