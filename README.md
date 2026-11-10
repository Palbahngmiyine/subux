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

Files in the input's `public/` directory are copied to the output root
unchanged, so `public/favicons/icon.png` becomes `/favicons/icon.png`. Markdown
there is copied, not rendered. Set `"publicDir"` in `subux.json` to use another
directory inside the input, or `null` to copy nothing.

- `public/_headers` is appended after the headers Subux generates.
- `public/favicon.svg` and `public/robots.txt` replace the generated defaults.
- Any other file that would overwrite generated output, even with different
  letter case, stops the build.
- Files and directories whose name starts with a dot, such as `.DS_Store`, are
  not copied. `.well-known` is the exception.
- Symbolic links and special files are rejected.

To link icons or a web app manifest from every page, list them in `headLinks`:

```json
"headLinks": [
  { "rel": "icon", "type": "image/png", "sizes": "32x32", "href": "/favicons/favicon-32x32.png" },
  { "rel": "apple-touch-icon", "sizes": "180x180", "href": "/favicons/apple-icon-180x180.png" },
  { "rel": "manifest", "href": "/manifest.json" }
]
```

`rel` is one of `icon`, `apple-touch-icon`, `apple-touch-icon-precomposed` or
`manifest`, and `href` must be a relative, `http:` or `https:` URL. Declaring an
`icon` replaces the default `/favicon.svg` link.

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
