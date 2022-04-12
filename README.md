# `mergit`

<img src="assets/logo.svg" align="right" alt="Mergit mascot" />

A merge bot using GitHub Checks to orchestrate bot actions.

Some feature highlights:

* Run CI for PRs as if they were merged with the base branch, catching semantic merge conflicts, before merging them
* Run merges to different base branches in parallel
* Run CI for PRs on command for testing, to not waste CI cycles on spurious pushes
* Store all state in GitHub status + check pages, so no database is needed

### Installation

Now, you need to install the GitHub app to the relevant repositories:

1. Add `.mergit.yaml` to the repo with the required CI statuses (if you already have CI enabled on a repo, this will be the statuses that show up on the commits), e.g.

    ```yaml
    statuses:
    - "ci/circleci: build"
    - "ci/circleci: test"
    ```

1. Edit CI config to only run CI on branches matching `staging-.*` or `trying-.*`, e.g.

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
    * Settings tab > Mergit > Install App > Settings icon > select desired repository
1. Make a PR
1. Do the following in GitHub for all protected branches:
    1. Remove all CI statuses from required status checks
    1. Set the "Merge" check as a required status check
    1. Ensure "Require linear history" is unchecked
    1. Ensure "Require branches to be up to date before merging" is unchecked (this is handled automatically by Mergit)
1. Use Mergit to merge in the PR!

#### Troubleshooting

* `This branch must not contain merge commits.`

    If you're getting this error, make sure to disable "Require linear history" for the base branch.
