workspace(name = "merge_bot")

load("//tools/build_rules:workspace.bzl", "github_archive")

github_archive(
    name = "io_tweag_rules_haskell",
    repo = ("tweag", "rules_haskell"),
    commit = "96315c6294ebe70abd492bed94d3fee172c0e15f",
    sha = "6f1617c8d1782d29081bcd3bf157d9dad3bf875dd85d05b9dc00233269707a9d",
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
load("@ai_formation_hazel//:hazel.bzl", "hazel_repositories")
load("//tools/build_rules:packages.bzl", "core_packages", "packages")
hazel_repositories(core_packages = core_packages, packages = packages)
