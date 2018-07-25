# LeapYear Merge Bot

## Quickstart

TODO

## Features

* Add/remove from a merge queue
    * Fails if a "do not merge" label is attached to the PR
    * If review is not green, put in a holding queue and auto-queue when review is green
    * Batch all PRs in the queue into a single job and bisect if job fails
    * Automatically merges PR when CI passes
        * Can customize merging algorithm (merge or squash/rebase)
* Add/remove from try jobs
    * Runs CI as if PR is in merge queue
    * Fails if PR already in merge queue/job
    * Cancels any currently running try jobs
* Cancel running jobs
* View queues and status of jobs

## Configuration

TODO
