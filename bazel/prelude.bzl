load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def github_archive(name, repo, version, release=True, sha=None):
    (repo_owner, repo_name) = repo
    archive = ("v" if release else "") + version + ".tar.gz"
    http_archive(
        name = name,
        strip_prefix = "{}-{}".format(repo_name, version),
        urls = ["https://github.com/{}/{}/archive/{}".format(repo_owner, repo_name, archive)],
        sha256 = sha,
    )
