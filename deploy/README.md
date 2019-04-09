# Merge Bot Deployment

This directory contains Terraform scripts to deploy the merge bot to AWS using
EC2 instances.

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
