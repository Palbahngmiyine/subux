# CLAUDE.md

This file provides guidance to Claude Code (`claude.ai/code`) and AI coding assistants when working with code in this repository.

## Project Overview

**Subux** (`subux`) is a pure functional Static Site Generator (SSG) written in **OCaml 5** with a zero-PPX native compilation pipeline:

- **W3C DTCG Design Token Engine** — Compiles the 3-layer design token system (`design/tokens.json` & `design/COLOR-THEME.md`) directly into `--sx-*` CSS custom properties for Light (`#fafafa`) and Dark (`#171717`) modes.
- **`Blog_mode`** — Tabbed collection views (`articles`, `tech`, `translations`), year-grouped timelines, series navigation, Obsidian `[[WikiLinks]]`, 27 Callout aliases mapped to 16 semantic tokens, `::video{...}` directives, footnote hover tooltips, `.hljs-*` syntax highlighting, lazy Mermaid rendering, and 404 recovery.
- **`Docs_mode`** — Technical documentation layout featuring `layout: home` hero/features landing pages, ordered/auto-generated sidebar navigation, right-hand document outline (`On this page` TOC), prev/next document pager, and client-side search modal (`⌘K`).
- **`Unified_mode`** — Hybrid mode rendering both blog collections and documentation pages under a single unified theme stylesheet.

## Commands

```bash
# Build native library and CLI executable (_build/ in-place, ~16 MB total footprint)
dune build

# Run the full unit & integration test suite (frontmatter safety, URL sanitizer, markdown, tokens, E2E site builds)
dune runtest

# Build a static site (defaults: -i . -o _site --mode blog --tone primary)
dune exec subux -- build [--mode blog|docs|unified] [--tone primary|accent] [-i <dir>] [-o <dir>] [--tokens <file>] [--title <str>]

# Compile W3C DTCG tokens (design/tokens.json) to a standalone CSS stylesheet
dune exec subux -- tokens [--tokens <file>] [--tone primary|accent] [-o <css-file>]

# Validate config, tokens, frontmatter, and markdown across a workspace without writing files
dune exec subux -- check [-i <dir>] [--tokens <file>]

# Scaffold a new blog or docs workspace
dune exec subux -- init [--mode blog|docs] <dir>
```

## Architecture

The codebase enforces **Functional Core, Imperative Shell**:

| Path | Layer | Responsibility |
| :--- | :--- | :--- |
| `lib/domain.mli`, `lib/domain.ml` | Pure Core | Immutable ADTs (`build_error`, `Result_syntax`, `Slug`, `date_val`, 16 `callout_kind` variants, `frontmatter_meta`, `site_config`, `default_blog_config`, `default_docs_config`) |
| `lib/safe_url.mli`, `lib/safe_url.ml` | Pure Core | WHATWG URL scheme allowlist (`http`, `https`, `mailto`, `tel`, relative `/`, `#`, `?`) & control-character stripper |
| `lib/json_parser.mli`, `lib/json_parser.ml` | Pure Core | Dependency-free RFC 8259 JSON parser and serializer |
| `lib/token_compiler.mli`, `lib/token_compiler.ml` | Pure Core | Compiles `design/tokens.json` (`palette.*`, `color.*`, `color.code.*`, `color.callout.*`) into `--sx-*` CSS variables and unified stylesheet |
| `lib/frontmatter.mli`, `lib/frontmatter.ml` | Pure Core | Safe YAML frontmatter subset parser (blocks `!!js/function` tags, anchors `&`, aliases `*`; supports blog & documentation hero/features metadata) |
| `lib/markdown.mli`, `lib/markdown.ml` | Pure Core | `Cmarkit` AST pipeline: `[[WikiLinks]]`, 27 Obsidian Callouts -> 16 Color tokens, `::video{...}` directive, footnote hover tooltips, `.hljs-*` syntax highlighting, lazy Mermaid, GFM, TOC & reading-time extraction |
| `lib/html.mli`, `lib/html.ml` | Pure Core | Type-safe HTML node ADT (`El`, `Void`, `Txt`, `Raw`, `Frag`, `Empty`) with automatic HTML entity escaping |
| `lib/site_builder.mli`, `lib/site_builder.ml` | Pure Core | Pure site compiler (`compile_site`) producing `output_artifact list` for `Blog_mode`, `Docs_mode`, `Unified_mode`, `search-index.json`, and `sitemap.xml` |
| `bin/main.ml` | Imperative Shell | CLI parser and all filesystem I/O (`Sys`, `Unix`, `In_channel`, `Out_channel`) |
| `vendor/cmarkit/` | Vendored Lib | Pure OCaml CommonMark parser (`cmarkit`, ISC license, zero external dependencies) |
| `design/` | Design System | `COLOR-THEME.md` specification and `tokens.json` W3C DTCG source of truth |
| `test/ssg_test.ml` | Verification | Comprehensive unit and end-to-end test suite |
| `LIGHTHOUSE-TASKS.md` | Local Backlog | *(Optional, git-ignored)* LLM-formatted task list for Lighthouse 100/100/100/100 optimization; read if present |

## Strict Functional Programming Rules

1. **Pure `lib/`, I/O-only `bin/`**:
   - Modules inside `lib/` (`Subux_lib`) must remain **100% pure functions** (`string -> ('a, Domain.build_error) result`). Never call `Sys`, `Unix`, `In_channel`, or `Out_channel` inside `lib/`.
   - All filesystem reads, directory traversals, and file writes belong exclusively in `bin/main.ml`.
2. **No Mutation or Exceptions for Control Flow**:
   - Do not use `ref`, `mutable` record fields, or in-place array mutation in domain/transformation logic (localized `Buffer.t` for final string serialization is permitted).
   - Do not use `raise`, `failwith`, `invalid_arg`, or `Not_found` for recoverable errors. Propagate all errors explicitly via `('a, Domain.build_error) result` and `open Domain.Result_syntax` (`let*`).
3. **Interface-First (`.mli`) & Parse, Don't Validate**:
   - Every `lib/<module>.ml` must have a matching `lib/<module>.mli` signature.
   - Parse raw strings into validated domain types (`Slug.t`, `Safe_url`, `callout_kind`, `date_val`) before passing them to HTML renderers.
4. **Compiler Strictness & Zero-PPX Disk Policy**:
   - All targets compile with `-w +a-4-40-42-44 -warn-error +a`. Avoid catch-all wildcard matches (`_ ->`) on domain variants so adding a new constructor triggers a compile-time exhaustiveness error.
   - **Never add PPX syntax extensions (`ppx_deriving`, `ppx_yojson_conv`, etc.) or C/Rust FFI dependencies.** Keeping the project pure OCaml + vendored `cmarkit` preserves sub-second incremental builds and keeps `_build/` around ~16 MB.

## Color Theme Rules (`design/COLOR-THEME.md`)

1. **Semantic Tokens Only**: Reference `--sx-*` CSS variables generated from `design/tokens.json`. Never hardcode hex/RGB literals or reference `palette.*` primitives directly in component rules.
2. **Pastel Accents Are Fill Colors, Never Text Colors**:
   - `--sx-color-primary` (`#c2e7ff`) and `--sx-color-accent` (`#ffdac2`) are **surface fills**.
   - Always pair them with `--sx-color-on-primary` (`#1f1f1f`) or `--sx-color-on-accent` (`#1f1f1f`) inside pill/badge containers (`--sx-active-fill` + `--sx-on-active-fill`).
   - For inline text links without a background fill (footnote refs, TOC links), always use `--sx-color-link` (`#1f6f9a` light / `#7fcfff` dark).
3. **Non-Color Signals (Accessibility)**:
   - Every state distinguished by color (active tab, active sidebar item, callout type, status badge) must also include a non-color indicator: bottom border/indicator bar, `font-weight: 600`, icon/symbol (`✓`, `!`, `✗`, `i`), or text label.
4. **Syntax Highlighting Isolation**:
   - `--sx-color-code-*` (`.hljs-*`) tokens are reserved strictly for `<pre><code>` blocks and must never be used for general UI elements.
