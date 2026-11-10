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

## Static files

Files in `public/` inside the input directory are copied to the site root
unchanged, so `site/public/favicons/favicon.ico` is served at
`/favicons/favicon.ico`. Dotfiles are skipped, except the `.well-known`
directory. The build fails when a static file would replace generated output
such as `index.html`, `_headers`, or `robots.txt`, and when `public/` contains a
symbolic link or a special file.

## Development

```sh
dune build
dune runtest
dune exec subux -- check
```

CI validates native Linux and macOS packages on x86_64 and arm64. Release Please
maintains release PRs; merging one publishes tested archives and checksums.
See [Releases](docs/RELEASING.md) for repository setup and recovery.

## License

Subux is licensed under the [MIT License](LICENSE). The vendored cmarkit
parser keeps its own license in
[`vendor/cmarkit/LICENSE.md`](vendor/cmarkit/LICENSE.md).
