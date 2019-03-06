workspace(name = "merge_bot")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

rules_haskell_version = "0.8"

http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-%s" % rules_haskell_version,
    urls = ["https://github.com/tweag/rules_haskell/archive/v%s.tar.gz" % rules_haskell_version],
    sha256 = "431d492a8ee6a110cdf42496181c9d27225dfb997379e64a148eb8e69f272ab7",
)

load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")

haskell_repositories()

load("@io_tweag_rules_haskell//haskell:ghc_bindist.bzl", "haskell_register_ghc_bindists")

haskell_register_ghc_bindists("8.6.3")
