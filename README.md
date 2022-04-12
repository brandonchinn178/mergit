# `mergit`

<img src="assets/logo.svg" align="right" alt="Mergit mascot" />

A merge bot using GitHub Checks to orchestrate bot actions.

Some feature highlights:

* Run CI for PRs as if they were merged with the base branch, catching semantic merge conflicts, before merging them
* Run merges to different base branches in parallel
* Run CI for PRs on command for testing, to not waste CI cycles on spurious pushes
* Store all state in GitHub status + check pages, so no database is needed

## Usage

After installing Mergit, this is the typical workflow you'll be using to run CI on your PRs + merge them.

First, you'd typically want to run your PR against CI, to check that your branch passes CI. Just like a normal merge, this will merge in the base branch, to ensure that CI would pass if your PR is merged today (even if you branched off the base branch a while ago + the base branch got updates). You'd go to the "Checks" tab in the PR, click on "Mergit App > Try", and click "Run Try". After a few seconds, GitHub should prompt you to refresh, and the Try run information should show up on the page.

After getting your PR approved + ready to merge, you'll want to queue the PR. Go to the "Checks" tab in the PR, click on "Merge App > Merge", and click "Queue". After a few seconds, GitHub should prompt you to refresh, and the check run should now say your PR is in the queue. Your PR should run and be merged automatically (batched with other queued PRs), without any additional steps on your part.

The status of your PRs can be inspected in the [Mergit Web UI](https://mergit.build-leapyear.com). Make sure to not click the "Reset merge run" button unless you're absolutely sure you want to do so ([#190](https://github.com/LeapYear/mergit/issues/190))

## Installing Mergit

1. Add `.mergit.yaml` to the repo with the required CI statuses (if you already have CI enabled on a repo, this will be the statuses that show up on the commits).

    e.g. for Circle CI:

    ```yaml
    statuses:
    - "ci/circleci: build"
    - "ci/circleci: test"
    ```

1. Ensure CI is configured to run on branches matching `staging-.*` or `trying-.*`.

    (Optional) To avoid running CI on every push (and only run CI when manually requested via "Run Try" or "Queue"), edit CI config to only run CI on branches matching `staging-.*` or `trying-.*`.

    e.g. for Circle CI:

    ```yaml
    _aliases:
      - &only-mergit
        filters:
          branches:
            only:
              - /staging-.*/
              - /trying-.*/

    workflows:
      my_workflow:
        jobs:
          - job1:
              <<: *only-mergit
          - job2:
              <<: *only-mergit
          - job3:
              <<: *only-mergit
              requires:
                - job1
                - job2
    ```

1. Have an admin add the repository to the GitHub app: https://github.com/apps/mergit-app

1. Create a Pull Request

1. Do the following in GitHub for all protected branches:
    1. Remove all CI statuses from required status checks
    1. Set the "Merge" check as a required status check
    1. Ensure "Require linear history" is unchecked
    1. Ensure "Require branches to be up to date before merging" is unchecked (this is handled automatically by Mergit)

1. Use Mergit to merge in the PR!

## Troubleshooting

* `This branch must not contain merge commits.`

    If you're getting this error, make sure to disable "Require linear history" for the base branch.
