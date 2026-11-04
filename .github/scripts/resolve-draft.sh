#!/usr/bin/env bash
# Resolve a draft's immutable commit; emit outputs only after every check passes.
set -euo pipefail

[[ "$RELEASE_TAG" =~ ^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]] || exit 1
release=$(gh release view "$RELEASE_TAG" --json isDraft,targetCommitish)
test "$(jq -r .isDraft <<< "$release")" = true
sha=$(jq -er .targetCommitish <<< "$release")
[[ "$sha" =~ ^[0-9a-f]{40}$ ]] || exit 1
tag_sha=$(gh api "repos/$GH_REPO/commits/$RELEASE_TAG" --jq .sha)
test "$tag_sha" = "$sha"
printf 'tag=%s\nsha=%s\n' "$RELEASE_TAG" "$sha" >> "$GITHUB_OUTPUT"
