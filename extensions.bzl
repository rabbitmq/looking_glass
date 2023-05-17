load(
    "@bazel_tools//tools/build_defs/repo:git.bzl",
    "new_git_repository",
)
load(
    "@bazel_tools//tools/build_defs/repo:utils.bzl",
    "maybe",
)

NIF_HELPERS_BUILD_FILE_CONTENT = """exports_files([
    "nif_helpers.h",
    "nif_helpers.c",
])
"""

def _external_deps(_ctx):
    maybe(
        repo_rule = new_git_repository,
        name = "nif_helpers",
        build_file_content = NIF_HELPERS_BUILD_FILE_CONTENT,
        commit = "ead6adc15fca3c314351523080d2ccb1956d5956",
        remote = "https://github.com/ninenines/nif_helpers",
    )

external_deps = module_extension(
    implementation = _external_deps,
)
