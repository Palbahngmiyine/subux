# Releases

The [CI workflow](../.github/workflows/ci.yml) builds, tests, checks content,
and smoke tests an extracted distribution on every PR and push to `main`,
including documentation changes. It then assembles the npm-format package from
the four tested archives and runs it as installed. It uses OCaml 5.3.0 and Dune
3.17.2 on:

| Package | GitHub runner |
| --- | --- |
| `linux-x86_64` | Ubuntu 22.04 x86_64 |
| `linux-arm64` | Ubuntu 22.04 arm64 |
| `darwin-x86_64` | macOS 15 Intel |
| `darwin-arm64` | macOS 15 Apple Silicon |

## Repository setup

Enable **Settings → Actions → General → Workflow permissions → Allow GitHub
Actions to create and approve pull requests**. The workflows use the automatic
`GITHUB_TOKEN`; no stored personal access token is required. An organization
policy can prevent enabling this setting.

GitHub requires a maintainer to select **Approve workflows to run** for CI on
PRs created or updated with `GITHUB_TOKEN`. Approve the release PR's checks
and merge it after they pass. See GitHub's
[workflow triggering rules](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/trigger-a-workflow#triggering-a-workflow-from-a-workflow).

Branch protection is configured separately. Require the four **Build and test**
checks if merges must wait for CI.

## Release flow

1. Merge changes using [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).
   With squash merging, preserve the conventional PR title in the commit title.
   `feat:` starts a minor release, `fix:` a patch, and breaking changes a minor
   release before 1.0 (`feat!:` or a `BREAKING CHANGE:` footer).
2. [Release Please](https://github.com/googleapis/release-please) opens or updates
   a release PR containing `version.txt`, `.release-please-manifest.json`, and
   `CHANGELOG.md`. The unreleased baseline is `0.0.0`; the first release is `0.1.0`.
3. Merging that PR reserves its version tag and creates a draft GitHub Release.
   Creating the tag also lets Release Please recognize the pending version.
   The [Release workflow](../.github/workflows/release.yml) validates the draft's
   exact commit on all four targets. A failed check leaves the release as a draft.
4. After checking the tag commit, archive checksums, embedded commit/version,
   and complete asset list, it uploads four archives, the npm-format package
   and `SHA256SUMS`, and publishes the draft.
   Publication stays in this workflow because token-created tags do not trigger
   another push workflow.

Each archive includes `subux`, `design/tokens.json`, both project and cmarkit
licenses, `version.txt`, `README.md`, and a `BUILD-INFO` commit/platform record.
Extract it and run its `subux` from any directory: a site without design tokens
of its own gets the ones beside the executable. OCaml and Dune are only needed
to build from source. Linux packages require glibc 2.35 or newer; macOS packages
target macOS 15. These are native builds, not cross-compilations.

`subux-<version>.tgz` is an npm-format package for sites that install Subux
with a Node package manager. Its `platforms/` directory holds the four archives
as tested, and its `subux` command runs the one for the current platform.

## Recovery

For a failed build or upload, use **Re-run failed jobs**, or run **Release** on
`main` with `recovery-tag` set to the existing draft tag (for example `v0.1.0`).
Recovery rebuilds and tests that draft's recorded commit, replaces only draft
assets, and publishes after every check passes. Published releases are rejected.
Normal release runs stop while a versioned draft exists, so finish its recovery
before starting the next release.
Leave the draft unpublished while investigating; do not manually publish it
without its assets. With an empty recovery tag, a manual run updates release PRs.
