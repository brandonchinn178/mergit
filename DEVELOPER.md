# Mergit developer docs

## Quickstart

First time install:

1. Go to `smee.io` and start a new channel

1. Set up a GitHub app for development

    1. Follow the instructions in the "Setting up a new GitHub App" section below with the following parameters:
        * Name: Mergit (Dev)
        * Homepage URL: `https://localhost:3000/`
        * Callback URL: `https://localhost:3000/auth/callback/`
        * Webhook URL: `https://smee.io/<token>`

    1. Set the following environment variables, preferably in a `.env` file based on `.env.template`:
        * `GITHUB_APP_ID` to the App ID
        * `GITHUB_CLIENT_ID` to the Client ID
        * `GITHUB_CLIENT_SECRET` to the Client secret
        * `GITHUB_WEBHOOK_SECRET` to the webhook secret you created
        * `GITHUB_PRIVATE_KEY` to the absolute path of the private key
        * `GITHUB_USER_AGENT` to the [user agent](https://docs.github.com/en/rest/overview/resources-in-the-rest-api#user-agent-required) to use with the API

1. Install [`smee`](https://github.com/probot/smee-client)

Run Mergit in development:

1. `stack build`
1. `source .env`
1. `smee --url https://smee.io/<token> --path /webhook/`
1. `stack exec mergit`

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

* `mergit-core`: The core logic for `mergit`.

* `mergit`: The web service that serves `mergit` as a GitHub app.

See [ARCHITECTURE.md](ARCHITECTURE.md) for more information.

## Deploy

1. Set up a GitHub app

    1. Follow the instructions in the "Setting up a new GitHub App" section below with the following parameters:
        * Name: Mergit
        * Homepage URL: `https://${DOMAIN}`
        * Callback URL: `https://${DOMAIN}/auth/callback/`
        * Webhook URL: `https://${DOMAIN}/webhook/`

1. Deploy the Mergit Docker image from Dockerhub (TODO: link)

    Run it in a Docker cluster with the following configuration:

    * Generate a private key to sign cookies + mount it into the Docker container

    * Mount the `.pem` private key file from the "Setting up a GitHub app" workflow into the Docker container

    * Set the following environment variables:

        * `GITHUB_APP_ID`: from "Setting up a GitHub app" workflow
        * `GITHUB_CLIENT_ID`: from "Setting up a GitHub app" workflow
        * `GITHUB_CLIENT_SECRET`: from "Setting up a GitHub app" workflow
        * `GITHUB_WEBHOOK_SECRET`: from "Setting up a GitHub app" workflow
        * `GITHUB_PRIVATE_KEY`: path to the GitHub app private key you mounted
        * `GITHUB_USER_AGENT`: the [user agent](https://docs.github.com/en/rest/overview/resources-in-the-rest-api#user-agent-required) to use with the API
        * `COOKIE_JWK`: path to the private key for signing cookies you mounted
        * `MERGIT_URL`: `https://${DOMAIN}`

## Appendix

### Setting up a new GitHub app

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

Reference: https://developer.github.com/apps/quickstart-guides/setting-up-your-development-environment/#step-2-register-a-new-github-app
