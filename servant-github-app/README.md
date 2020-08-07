# Servant combinators for a GitHub App

TODO: add in stuff from top-level README

## Run the example

1. Use Smee to tunnel HTTP requests to `localhost:3000`
    1. Go to `smee.io` and start a new channel
    1. Install `smee`: `npm install -g smee`
    1. Run `smee --url https://smee.io/<token> --path /webhook/`

1. Follow [these instructions][create-github-app] to create a GitHub app.
    1. Use the URL `smee.io` generated for you for the webhook URL.
    1. **Make sure to set a webhook secret!**
    1. Save the webhook secret you created and the App ID and Private key that
       GitHub generated for you.
    1. Generate a private key and download it

1. Set the following environment variables:
    1. `GITHUB_APP_ID` to the App ID
    1. `GITHUB_WEBHOOK_SECRET` to the webhook secret you created
    1. `GITHUB_PRIVATE_KEY` to the absolute path of the private key
    1. `GITHUB_USER_AGENT` to your GitHub username

1. Run example app
    1. `stack build servant-github-app`
    1. `stack exec servant-github-app-example`

1. Use example app
    1. Install the GitHub App to a repository to send an event
    1. Go to `localhost:3000` to view a Hello World page

[create-github-app]: https://developer.github.com/apps/quickstart-guides/setting-up-your-development-environment/#step-2-register-a-new-github-app

## Developer workflow

When developing your GitHub App using this package, the steps are the same as
running the example (including setting the environment variables), except
instead of running the example executable, you should run your project's
executable.

## Production

When deploying your GitHub App in production, create a new GitHub App pointing
to your deployed service. Make sure your deployed service has set the same
environment variables.
