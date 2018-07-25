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
* Cancel merge job
* Create/cancel try jobs
    * Runs CI as if PR is in merge queue
    * Fails if PR already in merge queue/job
    * Cancels any currently running try jobs
* View queues and status of jobs

## Usage

On the PR page in GitHub, add a comment with one of the following:

| Comment           | Description                 |
|-------------------|-----------------------------|
| lybot q+ [opts]   | Add to the merge queue      |
| lybot q-          | Remove from the merge queue |
| lybot try         | Create try job              |
| lybot cancel      | Cancel merge/try job        |

Options take the format `{ name = value, name = value }`. Available options:

* `type = merge|squash`: determine merging algorithm

## Configuration

TODO
