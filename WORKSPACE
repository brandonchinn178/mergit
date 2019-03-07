workspace(name = "merge_bot")

load("//tools/build_rules:workspace.bzl", "github_archive")

github_archive(
    name = "io_tweag_rules_haskell",
    repo = ("brandon-leapyear", "rules_haskell"),
    commit = "986e1bad2c9f0d9f13c7ac9ea37853cc0fb3a2ba",
    sha = "0d1d8a0087401f451fd54b44e106cde92372b0a07be6d3fa5b83bcf24e4072f5",
)

# load dependencies for 'rules_haskell'
load("@io_tweag_rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

# needed for haskell.bzl
github_archive(
    name = "io_tweag_rules_nixpkgs",
    repo = ("tweag", "rules_nixpkgs"),
    release = "0.2",
    sha = "2ff425a98762322eb895f092bb58effecb52bddc06b468ce1123d090e960e14d",
)

# Load GHC compiler
ghc_version = "8.6.3"
load("@io_tweag_rules_haskell//haskell:haskell.bzl", "haskell_register_ghc_bindists")
haskell_register_ghc_bindists(ghc_version)

# Hazel
github_archive(
    name = "ai_formation_hazel",
    repo = ("FormationAI", "hazel"),
    commit = "4684266e14e4a4ebb5973c1036f701f7f287d3fa",
    sha = "fd6a4542bfeb02793e8ff6a65d42eaed5bc98198e348428f4940be6f3a84707d",
)
load("@ai_formation_hazel//:hazel.bzl", "hazel_repositories", "hazel_custom_package_hackage")
load("//tools/build_rules:packages.bzl", "core_packages", "packages")
hazel_repositories(
    core_packages = core_packages,
    packages = packages,
    ghc_workspaces = {
        "darwin": "@io_tweag_rules_haskell_ghc_darwin_amd64",
    },
)

# Custom Hazel packages
# https://github.com/FormationAI/hazel/issues/81#issuecomment-470660846
hazel_custom_package_hackage(
    package_name = "clock",
    version = "0.7.2",
    sha256 = "886601978898d3a91412fef895e864576a7125d661e1f8abc49a2a08840e691f",
)
