# Merge Bot Deployment

This directory contains Terraform scripts to deploy the merge bot to AWS using
EC2 instances.

The `main.tf` Terraform script configures two groups of resources:

* resources for provisioning an EC2 instance to run the merge bot
* resources for setting up a load balancer for the EC2 instance

The merge bot will probably not have heavy traffic, but the load balancer
lets us use SSL from Amazon's certificate service, letting us accept HTTPS
requests from GitHub while routing HTTP requests internally to the merge bot.

## Pre-Requisites

1. Install [`tfenv`](https://github.com/tfutils/tfenv)
1. Get the app ID, webhook secret, and private key from LastPass.

## Deploy for the first time

1. Turn on the VPN
1. `cd` to this directory
1. `mkdir artifacts/`
1. Save the private key as `artifacts/github-app.pem`
1. Download the new `mergit` binary from Circle CI
    1. Find the appropriate workflow on https://app.circleci.com/pipelines/github/LeapYear/mergit
        - A common use case is to select the most recent successful build on the `main` branch
    1. Go to the `build` CI job
    1. Go to Artifacts, and download the `mergit` binary.
1. Create a `terraform.tfvars` file

    ```
    app_id = xxx
    client_id = xxx
    client_secret = xxx
    webhook_secret = "xxx"
    ```

1. `terraform init`
1. `terraform apply`
1. Try opening https://mergit.build-leapyear.com/ in your browser

## Inspect

If you want to get logs or otherwise inspect the EC2 instance running the merge
bot, run the following:

1. Turn on the VPN
1. `cd` to this directory
1. `terraform init`
1. `terraform apply`
    1. Make sure the EC2 instance isn't being changed; just that the keyfile
       is being created locally
1. `$(terraform output ssh_cmd)`

View the logs with `journalctl -u mergit`. You might want to pipe this into `less` and/or `grep` — see `man journalctl` for more information.

The merge bot process can be inspected with `systemctl status mergit`. You can manually restart the merge bot process with `systemctl restart mergit`

## Redeploy

TODO: Set up CD for the merge bot and remove this section (https://leapyear.atlassian.net/browse/QA-183)

Run the following steps to redeploy Mergit; e.g. after merging a PR that fixes a bug.

1. Turn on the VPN
1. Download the new `mergit` binary from Circle CI
    1. Find the appropriate workflow on https://app.circleci.com/pipelines/github/LeapYear/mergit
        - A common use case is to select the most recent successful build on the `main` branch
    1. Go to the `build` CI job
    1. Go to Artifacts, and download the `mergit` binary.
1. `cd` to this `deploy` directory
1. Populate `artifacts/github-app.pem` and `terraform.tfvars` from the Lastpass entry "Mergit",
if you don't already have them
1. Put the new `mergit` binary in `artifacts/`
1. `terraform init`
1. `terraform taint aws_instance.merge_bot`
1. `terraform apply`
1. Try opening https://mergit.build-leapyear.com/ in your browser
