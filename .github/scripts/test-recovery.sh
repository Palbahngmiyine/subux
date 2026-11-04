#!/usr/bin/env bash
set -euo pipefail

script="$(pwd)/.github/scripts/resolve-draft.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir "$work/bin"

# Accept only the two read-only requests needed to validate a recovery input.
cat > "$work/bin/gh" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_CALLS"
case "$*" in
  "release view $RELEASE_TAG --json isDraft,targetCommitish")
    printf '%s\n' "$MOCK_RELEASE"
    exit "$MOCK_VIEW_STATUS" ;;
  "api repos/$GH_REPO/commits/$RELEASE_TAG --jq .sha")
    printf '%s\n' "$MOCK_TAG_SHA"
    exit "$MOCK_API_STATUS" ;;
  *) echo 'Unexpected GitHub request' >&2; exit 99 ;;
esac
MOCK
chmod 755 "$work/bin/gh"
export PATH="$work/bin:$PATH"
export GH_REPO=fixture/subux GITHUB_OUTPUT="$work/output" MOCK_CALLS="$work/calls"
sha=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
draft="{\"isDraft\":true,\"targetCommitish\":\"$sha\"}"
printf 'tag=v0.1.0\nsha=%s\n' "$sha" > "$work/expected"

check_case() {
  local label=$1 expected=$2 status=0
  export RELEASE_TAG=$3 MOCK_RELEASE=$4 MOCK_TAG_SHA=$5
  export MOCK_VIEW_STATUS=${6:-0} MOCK_API_STATUS=${7:-0}
  rm -f "$GITHUB_OUTPUT"
  : > "$MOCK_CALLS"
  (cd "$work" && bash "$script") > "$work/log" 2>&1 || status=$?
  if [[ $expected == accept ]]; then
    if [[ $status != 0 ]] || ! diff -u "$work/expected" "$GITHUB_OUTPUT"; then
      cat "$work/log" >&2
      echo "FAIL: $label" >&2
      exit 1
    fi
  elif [[ $status == 0 || -e $GITHUB_OUTPUT ]]; then
    echo "FAIL: $label accepted or emitted release outputs" >&2
    exit 1
  fi
  echo "PASS: recovery validation: $label"
}

check_case 'valid draft' accept v0.1.0 "$draft" "$sha"
for tag in '' '0.1.0' 'v01.1.0' 'v0.1' '../v0.1.0' \
  'v0.1.0;touch injected' $'v0.1.0\nsha=injected'; do
  check_case 'malformed tag' reject "$tag" "$draft" "$sha"
  test ! -s "$MOCK_CALLS"
done
check_case 'published release' reject v0.1.0 \
  "{\"isDraft\":false,\"targetCommitish\":\"$sha\"}" "$sha"
check_case 'branch-valued target' reject v0.1.0 \
  '{"isDraft":true,"targetCommitish":"main"}' "$sha"
check_case 'mismatched tag commit' reject v0.1.0 "$draft" \
  bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
check_case 'missing draft state' reject v0.1.0 "{\"targetCommitish\":\"$sha\"}" "$sha"
check_case 'missing target commit' reject v0.1.0 '{"isDraft":true}' "$sha"
check_case 'malformed response' reject v0.1.0 'not JSON' "$sha"
check_case 'release lookup failure' reject v0.1.0 "$draft" "$sha" 1
check_case 'tag lookup failure' reject v0.1.0 "$draft" "$sha" 0 1
test ! -e "$work/injected"
