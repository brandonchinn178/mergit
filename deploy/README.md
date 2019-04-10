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

1. Install at least Terraform 0.11.13
1. Get the app ID, webhook secret, and private key from LastPass.
1. Download the `merge-bot` binary from a CI job.

## Steps

1. `cd` to this directory
1. `mkdir artifacts/`
1. Save the private key as `artifacts/github-app.pem`
1. Save the `merge-bot` binary as `artifacts/merge-bot`
1. Create a `terraform.tfvars` file

    ```
    app_id = xxx
    webhook_secret = "xxx"
    ```

1. `terraform init`
1. `terraform apply -auto-approve`
1. Upload the file at `terraform output keyfile` to LastPass

## Inspect

To SSH into the EC2 instance running the merge bot, download the keyfile
(saved as "LY Merge Bot (AWS)" on LastPass) and run

```
$(terraform output ssh_cmd) -i path/to/merge-bot-aws.pem
```
