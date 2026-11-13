#!/usr/bin/env bash
# Run from the checkout root after dune build. No compiler is needed at runtime.
set -euo pipefail

if [[ $# != 1 ]]; then
  echo 'Usage: package.sh OUTPUT_DIRECTORY' >&2
  exit 2
fi

version=$(cat version.txt)
if [[ ! $version =~ ^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]]; then
  echo 'Invalid release version' >&2
  exit 2
fi
case "$(uname -s)/$(uname -m)" in
  Linux/x86_64) platform=linux-x86_64 ;;
  Linux/aarch64) platform=linux-arm64 ;;
  Darwin/x86_64) platform=darwin-x86_64 ;;
  Darwin/arm64) platform=darwin-arm64 ;;
  *) echo 'Unsupported release platform' >&2; exit 2 ;;
esac

mkdir -p "$1"
output=$(cd "$1" && pwd)
# macOS tar would otherwise store extended attributes as "._" files.
export COPYFILE_DISABLE=1
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
name="subux-${version}-${platform}"
package="$work/$name"
mkdir -p "$package/design" "$package/vendor/cmarkit"
cp _build/default/bin/main.exe "$package/subux"
chmod 755 "$package/subux"
cp design/tokens.json "$package/design/tokens.json"
cp LICENSE README.md version.txt "$package/"
cp vendor/cmarkit/LICENSE.md "$package/vendor/cmarkit/LICENSE.md"
printf 'commit=%s\nplatform=%s\n' "$(git rev-parse HEAD)" "$platform" > "$package/BUILD-INFO"
archive="$name.tar.gz"
tar -czf "$output/$archive" -C "$work" "$name"
(cd "$output" && shasum -a 256 "$archive" > "$archive.sha256")

# Exercise the bytes users download, with no fallback to checkout-relative tokens.
mkdir "$work/extracted"
tar -xzf "$output/$archive" -C "$work/extracted"
(
  cd "$work/extracted/$name"
  ./subux help
  ./subux init --mode docs site
  ./subux check -i site
  ./subux build -i site -o _site
  test -s _site/index.html
)
echo "Packaged and smoke tested $archive"
