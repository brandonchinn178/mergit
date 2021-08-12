# LeapYear Merge Bot

## Quickstart

1. `stack build`
1. `source .env`
1. Go to `smee.io` and start a new channel
1. Install [`smee`](https://github.com/probot/smee-client)
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

## Development

### Build

`stack build`

### Regenerate graphql files

```bash
scripts/codegen.sh
```

### Pre-commit hooks

To enable pre-commit hooks, do the following:

1. Install [pre-commit](https://pre-commit.com)
1. `stack build hlint fourmolu`
1. `pre-commit install`

These are also checked in CI, so setting this up is optional, but highly recommended.

## Repo layout

The project is broken up into multiple sub-projects, most of which can be moved
out into a separate repo:

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

See [ARCHITECTURE.md](ARCHITECTURE.md) for more information.

## Configuration

1. Follow [these instructions][create-github-app] to create a GitHub app.
    1. Set homepage URL, callback URL, and webhook URL
    1. Generate a random webhook secret
    1. Installed only on this account
    1. Permissions:
        * Checks: Read & Write
        * Repository contents: Read & Write
        * Repository metadata: Read-only
        * Pull requests: Read & Write
        * Commit statuses: Read-only
    1. Events:
        * Check run
        * Pull request
        * Push
        * Status
    1. Save the webhook secret you created and the App ID, Client ID,
       Client secret, and Private key that GitHub generated for you.

1. Set the following environment variables, preferably in a `.env` file based
   on `.env.template`:
    1. `GITHUB_APP_ID` to the App ID
    1. `GITHUB_CLIENT_ID` to the Client ID
    1. `GITHUB_CLIENT_SECRET` to the Client secret
    1. `GITHUB_WEBHOOK_SECRET` to the webhook secret you created
    1. `GITHUB_PRIVATE_KEY` to the absolute path of the private key
    1. `GITHUB_USER_AGENT` to the [user agent][user-agent] to use with the API

[create-github-app]: https://developer.github.com/apps/quickstart-guides/setting-up-your-development-environment/#step-2-register-a-new-github-app
[user-agent]: https://developer.github.com/v3/#user-agent-required

### Development

To configure a merge bot GitHub app for development, follow the instructions in
the "Configuration" section to create a GitHub app with the following
parameters:

* Name: LeapYear Merge Bot (Dev)
* Homepage URL: `https://localhost:3000/`
* Callback URL: `https://localhost:3000/auth/callback/`
* Webhook URL: the smee URL generated for you

### Production

To configure a merge bot GitHub app for production, follow the instructions in
the "Configuration" section to create a GitHub app with the following
parameters:

* Name: LeapYear Merge Bot
* Homepage URL: `https://merge-bot.build-leapyear.com/`
* Callback URL: `https://merge-bot.build-leapyear.com/auth/callback/`
* Webhook URL: `https://merge-bot.build-leapyear.com/webhook/`

Make sure to save the webhook secret, app ID, and private key to LastPass.
Follow the instructions in `deploy/README.md` with these values.

The GitHub app should now be set up!

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

1. Edit CI config to only run CI on branches matching `staging-.*` or `trying-.*`
1. Make a PR
1. Set the "Bot Merge" check as a required status check (for protected branches)
1. Uncheck "Require branches to be up to date before merging"
1. Use the merge bot to merge in the PR!

#### Troubleshooting

* `This branch must not contain merge commits.`

    If you're getting this error, make sure to disable "Require linear history" for the base branch.

## Useful links

* [Runbook](https://www.notion.so/leapyear/Merge-Bot-Runbook-651a19edfff64217afd12b199a4491b2)
* [JIRA tracker](https://leapyear.atlassian.net/issues/?filter=10814)
