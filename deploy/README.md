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
1. Download the `merge-bot` binary from a CI job.

## Deploy for the first time

1. Turn on the VPN
1. `cd` to this directory
1. `mkdir artifacts/`
1. Save the private key as `artifacts/github-app.pem`
1. Save the `merge-bot` binary as `artifacts/merge-bot`
1. Create a `terraform.tfvars` file

    ```
    app_id = xxx
    client_id = xxx
    client_secret = xxx
    webhook_secret = "xxx"
    ```

1. `terraform init`
1. `terraform apply`

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

View the logs with `journalctl -u merge-bot`. You might want to pipe this into `less` and/or `grep` — see `man journalctl` for more information.

The merge bot process can be inspected with `systemctl status merge-bot`. You can manually restart the merge bot process with `systemctl restart merge-bot`

## Redeploy

TODO: Set up CD for the merge bot and remove this section (https://leapyear.atlassian.net/browse/QA-183)

The `github-app.pem` private key and the `terraform.tfvars` values should be
in LastPass under "LY Merge Bot".

1. Turn on the VPN
1. `cd` to this directory
1. Ensure that `artifacts/github-app.pem` still exists
1. Put the new `merge-bot` binary in `artifacts/`
1. `terraform taint aws_instance.merge_bot`
1. `terraform apply -auto-approve`
