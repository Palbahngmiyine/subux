#!/bin/sh
set -eu
case "$1" in /*) subux=$1 ;; *) subux=$PWD/$1 ;; esac
case "$2" in /*) tokens=$2 ;; *) tokens=$PWD/$2 ;; esac
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM
cd "$work"
mkdir -p input/articles input/contents
printf '%s\n' '# Home' > input/index.md
printf '%s\n' '# Article' > input/articles/post.md
printf '%s\n' '# Retained article' > input/articles/keep.md
printf '%s\n' '# Guide' > input/contents/guide.md
printf '%s\n' '{"baseUrl":"https://example.com"}' > input/subux.json

build() { "$subux" build -i input --tokens "$tokens" "$@"; }
reject() {
  if "$@" > error.txt 2>&1; then
    echo "Expected command failure: $*" >&2
    exit 1
  fi
}
for mode in blog docs unified; do
  build --mode "$mode" -o "static-$mode"
  build --mode "$mode" --adapter cloudflare --name my-subux -o "cf-$mode"
  diff -r "static-$mode" "cf-$mode/.cloudflare/output/v0/workers/default/assets"
done

# Deleted and drafted pages must disappear; unrelated output files survive.
printf '%s\n' keep > cf-blog/keep.txt
test -f cf-blog/.cloudflare/output/v0/workers/default/assets/post/index.html
rm input/articles/post.md
build --adapter cloudflare -o cf-blog/
test ! -e cf-blog/.cloudflare/output/v0/workers/default/assets/post/index.html
test "$(cat cf-blog/keep.txt)" = keep
printf '%s\n' '# Published' > input/articles/published.md
build --adapter cloudflare -o cf-blog
test -f cf-blog/.cloudflare/output/v0/workers/default/assets/published/index.html
printf '%s\n' '---' 'draft: true' '---' '# Draft' > input/articles/published.md
build --adapter cloudflare -o cf-blog
test ! -e cf-blog/.cloudflare/output/v0/workers/default/assets/published/index.html

# Validation/check must never write output, including for invalid CLI values.
"$subux" check -i input --tokens "$tokens" --adapter cloudflare -o checked
test ! -e checked
reject build --adapter typo -o invalid
reject build --adapter cloudflare --name '../private' -o invalid
reject build --adapter cloudflare --compatibility-date 2026-02-29 -o invalid
reject build --adapter
reject build --name
reject build --compatibility-date
test ! -e invalid

# A failed compile must preserve the last successful Cloudflare build.
cp cf-blog/.cloudflare/output/v0/workers/default/assets/index.html before.html
printf '%s\n' '---' 'title: !!js/function unsafe' '---' > input/articles/unsafe.md
reject build --adapter cloudflare -o cf-blog
cmp before.html cf-blog/.cloudflare/output/v0/workers/default/assets/index.html
rm input/articles/unsafe.md

# Refuse to erase output not owned by this adapter, or follow output symlinks.
mkdir -p unowned/.cloudflare/output/v0 outside
printf '%s\n' keep > unowned/.cloudflare/output/v0/keep.txt
reject build --adapter cloudflare -o unowned
test "$(cat unowned/.cloudflare/output/v0/keep.txt)" = keep
ln -s "$work/outside" linked
reject build --adapter cloudflare -o linked
reject build --adapter cloudflare -o linked/
reject build --adapter cloudflare -o linked/child
mkdir nested
ln -s "$work/outside" nested/.cloudflare
reject build --adapter cloudflare -o nested
test -z "$(ls -A outside)"
test -z "$(find cf-blog/.cloudflare/output -name '.subux-*' -type d)"
echo 'PASS: Cloudflare CLI builds, rebuilds, validation and output protection'
