#!/usr/bin/env bash
# Assemble the npm-format package from the four tested platform archives and
# smoke test it as installed. Run from the checkout root; needs Node and npm.
set -euo pipefail

if [[ $# != 2 ]]; then
  echo 'Usage: npm-package.sh ARCHIVE_DIRECTORY OUTPUT_DIRECTORY' >&2
  exit 2
fi

version=$(cat version.txt)
if [[ ! $version =~ ^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$ ]]; then
  echo 'Invalid release version' >&2
  exit 2
fi
if [[ ! -d $1 ]]; then
  echo 'Missing archive directory' >&2
  exit 2
fi
archives=$(cd "$1" && pwd)
# macOS tar would otherwise store extended attributes as "._" files.
export COPYFILE_DISABLE=1
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
package="$work/package"
mkdir -p "$package/bin" "$package/platforms"

# Each platform directory is a release archive exactly as it was tested.
for platform in linux-x86_64 linux-arm64 darwin-x86_64 darwin-arm64; do
  name="subux-${version}-${platform}"
  if [[ ! -s $archives/$name.tar.gz || ! -s $archives/$name.tar.gz.sha256 ]]; then
    echo "Missing archive: $name.tar.gz" >&2
    exit 2
  fi
  if ! (cd "$archives" && shasum -a 256 -c "$name.tar.gz.sha256") > /dev/null 2>&1; then
    echo "Checksum mismatch: $name.tar.gz" >&2
    exit 2
  fi
  mkdir "$work/$platform"
  tar -xzf "$archives/$name.tar.gz" -C "$work/$platform"
  test -x "$work/$platform/$name/subux"
  grep -Fxq "$version" "$work/$platform/$name/version.txt"
  grep -Fxq "platform=$platform" "$work/$platform/$name/BUILD-INFO"
  mv "$work/$platform/$name" "$package/platforms/$platform"
done

cp npm/bin/subux.js "$package/bin/subux.js"
chmod 755 "$package/bin/subux.js"
cp LICENSE README.md "$package/"
node -e '
  const fs = require("node:fs")
  const [source, target, version] = process.argv.slice(1)
  const manifest = JSON.parse(fs.readFileSync(source, "utf8"))
  manifest.version = version
  fs.writeFileSync(target, JSON.stringify(manifest, null, 2) + "\n")
' npm/package.json "$package/package.json" "$version"

mkdir -p "$2"
output=$(cd "$2" && pwd)
archive="subux-${version}.tgz"
tar -czf "$output/$archive" -C "$work" package
(cd "$output" && shasum -a 256 "$archive" > "$archive.sha256")

# Install the bytes users download into an empty project, then run this
# platform's executable from there, where no design tokens exist.
mkdir "$work/project"
(
  cd "$work/project"
  printf '{ "private": true }\n' > package.json
  npm install --no-audit --no-fund --ignore-scripts "$output/$archive"
  ./node_modules/.bin/subux help
  ./node_modules/.bin/subux init --mode docs site
  ./node_modules/.bin/subux check -i site
  ./node_modules/.bin/subux build -i site -o _site
  test -s _site/index.html
)
echo "Packaged and smoke tested $archive"
