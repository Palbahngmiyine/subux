#!/usr/bin/env bash
set -euo pipefail

script="$(pwd)/.github/scripts/package.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# Bad metadata must fail before creating an output directory or reading the binary.
for version in '../escape' '1.2.3/../../escape' '1.2.3;touch injected' '-1.2.3' \
  '1.2' '01.2.3' '1.02.3' '1.2.03' $'1.2.3\n4.5.6' ''; do
  printf '%s\n' "$version" > "$work/version.txt"
  if (cd "$work" && bash "$script" "$work/output") > "$work/log" 2>&1; then
    echo "Accepted invalid version: $version" >&2
    exit 1
  fi
  grep -Fxq 'Invalid release version' "$work/log"
  test ! -e "$work/output"
  test ! -e "$work/injected"
done
if bash "$script" > "$work/log" 2>&1; then
  echo 'Accepted missing output directory' >&2
  exit 1
fi
grep -Fxq 'Usage: package.sh OUTPUT_DIRECTORY' "$work/log"
echo 'PASS: package rejects invalid versions and missing arguments'
