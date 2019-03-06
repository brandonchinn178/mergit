load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def github_archive(name, repo, commit=None, release=None, sha=None):
    if not commit and not release:
        fail('Must specify either commit or release')
    elif commit and release:
        fail('Cannot specify both commit and release')
    elif commit:
        ref = commit
        archive = "{}.tar.gz".format(commit)
    else:
        ref = release
        archive = "v{}.tar.gz".format(release)

    (repo_owner, repo_name) = repo

    http_archive(
        name = name,
        strip_prefix = "{}-{}".format(repo_name, ref),
        urls = ["https://github.com/{}/{}/archive/{}".format(repo_owner, repo_name, archive)],
        sha256 = sha,
    )
