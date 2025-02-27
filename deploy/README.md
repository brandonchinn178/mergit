# Mergit Deployment

This directory contains Terraform scripts to deploy Mergit to AWS using
EC2 instances.

The `main.tf` Terraform script configures two groups of resources:

* resources for provisioning an EC2 instance to run Mergit
* resources for setting up a load balancer for the EC2 instance

Mergit will probably not have heavy traffic, but the load balancer
lets us use SSL from Amazon's certificate service, letting us accept HTTPS
requests from GitHub while routing HTTP requests internally to Mergit.

## Pre-Requisites

1. Install [`tfenv`](https://github.com/tfutils/tfenv)
1. [Install Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)
1. Get the app ID, webhook secret, and private key from LastPass.

## Deploy for the first time

1. Turn on the VPN
1. `cd` to this directory
1. `mkdir artifacts/`
1. Save the private key as `artifacts/github-app.pem`
1. Download the new `mergit` binary from Circle CI
    1. Find the appropriate workflow on https://app.circleci.com/pipelines/github/LeapYear/mergit
        - A common use case is to select the most recent successful build on the `main` branch
    1. Go to the `build_and_test` CI job
    1. Go to Artifacts, and download the `mergit` binary.
    1. Put the `mergit` binary in the `artifacts/` directory
1. Create a `terraform.tfvars` file

    ```
    app_id = xxx
    client_id = xxx
    client_secret = xxx
    webhook_secret = "xxx"
    ```

1. `terraform init`
1. `terraform apply`
1. `ansible-playbook -i ansible_hosts install-mergit.yml`
1. Try opening https://mergit.build-leapyear.com/ in your browser

## Inspect

If you want to get logs or otherwise inspect the EC2 instance running Mergit, run the following:

1. Turn on the VPN
1. `cd` to this directory
1. `terraform init`
1. `terraform apply`
    1. Make sure the EC2 instance isn't being changed; just that the keyfile
       is being created locally
1. `$(terraform output ssh_cmd)`

View the logs with `journalctl -u mergit`. You might want to pipe this into `less` and/or `grep` — see `man journalctl` for more information.

The `mergit` process can be inspected with `systemctl status mergit`. You can manually restart the `mergit` process with `systemctl restart mergit`

## Redeploy

TODO: Set up CD and remove this section (https://github.com/LeapYear/mergit/issues/194)

Run the following steps to redeploy Mergit; e.g. after merging a PR that fixes a bug.

1. Turn on the VPN
1. Download the new `mergit` binary from Circle CI
    1. Find the appropriate workflow on https://app.circleci.leapyear.io/pipelines/github/LeapYear/mergit
        - A common use case is to select the most recent successful build on the `main` branch
    1. Go to the `build_and_test` CI job
    1. Go to Artifacts, and download the `mergit` binary.
1. `cd` to this `deploy` directory
1. Populate `artifacts/github-app.pem` and `terraform.tfvars` from the Lastpass entry "Mergit",
if you don't already have them
1. Put the new `mergit` binary in `artifacts/`
1. `terraform init`
1. `terraform plan`:  only the local keyfile and `ansible_hosts` should normally change.  If no changes are indicated, your local files should be up to date.
1. `terraform apply -auto-approve`
1. `ansible-playbook -i ansible_hosts install-mergit.yml`
1. Try opening https://mergit.build-leapyear.com/ in your browser
