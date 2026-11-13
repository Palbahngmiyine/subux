#!/usr/bin/env bash
set -euo pipefail

script="$(pwd)/.github/scripts/npm-package.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir "$work/archives"

# Bad input must fail with its own message before an output directory exists.
reject() {
  local label=$1 message=$2
  shift 2
  if (cd "$work" && bash "$script" "$@") > "$work/log" 2>&1; then
    echo "Accepted $label" >&2
    exit 1
  fi
  grep -Fxq "$message" "$work/log"
  test ! -e "$work/output"
  test ! -e "$work/injected"
}

printf '1.2.3\n' > "$work/version.txt"
reject 'missing arguments' 'Usage: npm-package.sh ARCHIVE_DIRECTORY OUTPUT_DIRECTORY'
reject 'missing archive directory' 'Missing archive directory' "$work/absent" "$work/output"
for version in '../escape' '1.2.3;touch injected' '1.2' '01.2.3' $'1.2.3\n4.5.6' ''; do
  printf '%s\n' "$version" > "$work/version.txt"
  reject "invalid version: $version" 'Invalid release version' "$work/archives" "$work/output"
done

printf '1.2.3\n' > "$work/version.txt"
reject 'missing platform archive' 'Missing archive: subux-1.2.3-linux-x86_64.tar.gz' \
  "$work/archives" "$work/output"
printf 'archive\n' > "$work/archives/subux-1.2.3-linux-x86_64.tar.gz"
printf '%064d  subux-1.2.3-linux-x86_64.tar.gz\n' 0 > "$work/archives/subux-1.2.3-linux-x86_64.tar.gz.sha256"
reject 'archive that differs from its checksum' 'Checksum mismatch: subux-1.2.3-linux-x86_64.tar.gz' \
  "$work/archives" "$work/output"
echo 'PASS: npm package rejects invalid versions, missing archives and checksum mismatches'
