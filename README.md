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

The status of your PRs can be inspected in the Mergit Web UI. Make sure to not click the "Reset merge run" button unless you're absolutely sure you want to do so ([#190](https://github.com/LeapYear/mergit/issues/190))

## Installing Mergit

1. Deploy Mergit in your own infrastructure and create a GitHub app for it (see the "Deploy" section in `DEVELOPER.md`) or use the publicly hosted one (TBD)

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

1. Have an admin add the repository to the GitHub app

1. Create a Pull Request

1. Do the following in GitHub for all protected branches:
    1. Remove all CI statuses from required status checks
    1. Set the "Merge" check as a required status check
    1. Ensure "Require linear history" is unchecked
    1. Ensure "Require branches to be up to date before merging" is unchecked (this is handled automatically by Mergit)

1. Use Mergit to merge in the PR!

## Troubleshooting

### Merge run failed at "Checkout code"

In CI, you might see your merge run fail when checking out your repo with an error message like:

* `Couldn't find remote ref staging-main`
* `Could not parse object '<sha>'`
* `reference is not a tree: <sha>`

This means that Mergit was notified that the merge run failed, causing the CI branch + merge commit to be deleted and not available when a CI job tries to check out the branch.

The following scenarios would result in this:

* Say jobs A, B, and C all take the same amount of time, where C depends on B. If A fails but B succeeds, C will start and attempt to check out the repo, but A's failure told Mergit to clean up the CI branch, so C has no branch to check out.

* (Circle CI) This job or another job was restarted by Circle CI due to Circle CI infrastructure problems. Unfortunately, when Circle CI restarts a job, it sends a "failed" notification to GitHub.

    If this happens, you might see a message in Circle CI like "Parallel Run 1: There was an issue while running this parallel run and it was rerun". There's not much we can do about this, so you'll just have to retry.

### The "Merge" check succeeded but the PR didn't merge

Mergit is somehow in a bad state. Make a [GitHub issue](https://github.com/LeapYear/mergit/issues), and then do the following:

1. Go to the Mergit Web UI
1. Check if Mergit still thinks your PR is running. If so, click the "Reset staging branch" button.
    1. At this point, the staging branch should be deleted. Make sure this is the case (e.g. if your PR is merging into `main`, look for the `staging-main` branch)
1. Click the "Reset" button in the "Merge" checks page
1. "Queue" the PR again

### Nothing's happening when I do some action (e.g. Queue, Run Try)

First, make sure you're on the checks page for the latest commit in the PR. If you've verified that you are, and you're still having issues, try pushing a new commit. You can do one of the following:

- Make a trivial change and push
- Amend the commit message and force push
- Make an empty commit (`git commit —-allow-empty`) and push

### I pushed a commit, but nothing is showing up in the "Checks" tab

Try pushing a new commit. See previous section.

### Commit XXX found in multiple PRs

Something went horribly wrong, a mixture of GitHub not sending the correct payload and the commit ambiguously associated with multiple PRs. Open a [GitHub issue](https://github.com/LeapYear/mergit/issues).

### This branch must not contain merge commits.

If you're getting this error, make sure to disable "Require linear history" for the base branch.
