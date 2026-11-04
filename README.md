# Subux

Subux builds a Markdown workspace into a static website from one native
executable. It renders a blog, documentation, or both in one site (`blog`,
`docs`, and `unified` modes), styled from the design tokens in
[`design/tokens.json`](design/tokens.json).

## Requirements

- OCaml 5 and Dune 3.16 or later. Nothing else: the CommonMark parser is
  vendored and Subux has no opam dependencies.

## Getting started

Run these from the repository root:

```sh
dune build
dune exec subux -- init --mode docs site
dune exec subux -- build -i site -o _site
```

Set `baseUrl` in `site/subux.json` to the real site address so canonical URLs
and the sitemap use it. `dune exec subux -- help` lists every command and option.

## Cloudflare

`build --adapter cloudflare` writes a Workers Static Assets build to
`_site/.cloudflare/output/v0/`. Subux never deploys or handles credentials;
deploy that output yourself with the Cloudflare CLI (tested with cf
1.0.0-beta.25):

```sh
dune exec subux -- build --adapter cloudflare --name my-site -i site -o _site
cd _site
cf deploy --prebuilt --dry-run
cf deploy --prebuilt
```

## Development

```sh
dune build
dune runtest
dune exec subux -- check
```

## License

Subux is licensed under the MIT License, as declared in `dune-project`. The
vendored cmarkit parser keeps its own license in
[`vendor/cmarkit/LICENSE.md`](vendor/cmarkit/LICENSE.md).
