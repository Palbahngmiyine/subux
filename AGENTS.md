# AGENTS.md — AI Agent Engineering & Contribution Guide

This document defines the architectural invariants, security boundaries, design-token rules, and verification workflows for AI coding agents (OpenAI Codex, Google Antigravity/Gemini, Claude Code, Cursor, etc.) and contributors working on **Subux**.

---

## 1. Mission & Core Constraints

**Subux** is a pure functional Static Site Generator (SSG) built with **OCaml 5 (`ocamlopt`)** and **Dune**. It compiles markdown workspaces into static websites supporting three built-in rendering modes:

1. **`Blog_mode`** — Collection tabs (`articles`, `tech`, `translations`), year timeline grouping, reading time, TOC, series navigation, Obsidian `[[WikiLinks]]`, 27 Callout aliases mapped to 16 semantic tokens, `::video{...}` directives, footnote hover tooltips, `.hljs-*` syntax highlighting, lazy Mermaid rendering, client-side search, and 404 recovery.
2. **`Docs_mode`** — Documentation layout with `layout: home` hero/features landing pages, ordered/auto-generated `.jt-sidebar`, document outline TOC (`On this page`), prev/next footer pager, and search modal (`⌘K`).
3. **`Unified_mode`** — Hybrid mode rendering both blog collections and documentation pages under a single unified W3C DTCG theme stylesheet.

### Non-Negotiable Engineering Constraints

- **Minimal Disk Footprint (Zero-PPX Policy)**:
  - Do **not** introduce PPX syntax extensions (`ppxlib`, `ppx_deriving`, `ppx_yojson_conv`), C stubs, or external opam runtime dependencies.
  - The entire `_build/` directory must stay lightweight (~16 MB) and compile to a single native executable (`~3.2 MB`) in sub-second incremental builds.
  - `vendor/cmarkit/` is vendored in-tree so the project builds out-of-the-box with only the OCaml compiler and Dune installed.
- **Functional Core, Imperative Shell**:
  - `lib/` (`Subux_lib`) is **100% pure functional**. No filesystem, network, environment variable, or clock access is permitted in `lib/`.
  - `bin/main.ml` is the **only** place allowed to perform I/O (`Sys`, `Unix`, `In_channel`, `Out_channel`).
- **Compiler-Enforced Totality**:
  - All targets use `(flags (:standard -w +a-4-40-42-44 -warn-error +a))`. Warnings are fatal errors.

---

## 2. Repository Map

```text
subux/
├── dune-project              # Dune 3.16+ project metadata (package: subux)
├── CLAUDE.md                 # Quick reference for Claude Code
├── AGENTS.md                 # Canonical agent & contributor handbook (this file)
├── design/
│   ├── COLOR-THEME.md        # Subux Color Theme specification (Light/Dark, WCAG, LLM rules)
│   └── tokens.json           # W3C DTCG 3-layer design tokens (source of truth)
├── vendor/
│   └── cmarkit/              # Vendored pure-OCaml CommonMark parser (ISC license, zero deps)
├── lib/                      # Pure Functional Core (Subux_lib) — NO I/O ALLOWED
│   ├── dune
│   ├── domain.mli / .ml      # Core ADTs, Slug, date_val, 16 callout_kind variants, Result_syntax
│   ├── safe_url.mli / .ml    # WHATWG URL sanitizer (blocks javascript:, data:, vbscript:)
│   ├── json_parser.mli / .ml # Pure RFC 8259 JSON parser & serializer
│   ├── token_compiler.mli/.ml# Compiles design/tokens.json -> --sx-* CSS stylesheet
│   ├── frontmatter.mli / .ml # Safe YAML subset parser (blog + documentation hero/features)
│   ├── markdown.mli / .ml    # Cmarkit AST mapper & renderer (WikiLinks, Callouts, Footnotes, TOC)
│   ├── html.mli / .ml        # Type-safe HTML ADT with automatic entity escaping
│   └── site_builder.mli /.ml # Pure site compiler (Blog_mode, Docs_mode, Unified_mode)
├── bin/                      # Imperative Shell — I/O isolated here
│   ├── dune
│   └── main.ml               # CLI entry point (build, tokens, check, init, help)
└── test/
    ├── dune
    └── ssg_test.ml           # Unit & integration regression suite
```

---

## 3. Coding & Security Invariants

### 3.1 Pure Error Handling (`Result_syntax`)
Never throw exceptions (`raise`, `failwith`, `invalid_arg`, `Not_found`) in `lib/`. Use `Domain.build_error` and monadic binding:

```ocaml
open Domain
open Result_syntax

let transform (input : string) : (output, build_error) result =
  let* doc = Frontmatter.parse input in
  let rendered = Markdown.render doc.body in
  Ok (doc.meta, rendered)
```

### 3.2 Interface-First Development (`.mli`)
When adding or modifying a function or type in `lib/<name>.ml`:
1. Update `lib/<name>.mli` first with explicit type signatures and doc comments `(** ... *)`.
2. Keep internal helper functions private by omitting them from `lib/<name>.mli`.
3. Disambiguate record labels with explicit type annotations `(x : my_record)` so Warning 41 (`ambiguous-name`) is never triggered.

### 3.3 Security Boundaries
Every markdown transformation or template modification must preserve these three security defenses:
1. **Safe YAML Frontmatter (`lib/frontmatter.ml`)**:
   - Reject YAML tags (`!!js/function`, `!<tag>`), anchors (`&anchor`), and aliases (`*alias`) with `Error (Unsafe_yaml_construct ...)` or `Error (Invalid_frontmatter ...)`.
2. **WHATWG URL Sanitization (`lib/safe_url.ml`)**:
   - Every `<a href="...">`, `<img src="...">`, and `::video{src="..."}` attribute must pass through `Safe_url.is_safe_url` / `Safe_url.sanitize_url`.
   - Strip ASCII control characters (`0x00`–`0x1F`, `0x7F`) and whitespace before inspecting the scheme. Only `http:`, `https:`, `mailto:`, `tel:`, and relative URLs (`/`, `./`, `../`, `#`, `?`) are allowed.
3. **HTML Escaping (`lib/html.ml`)**:
   - Construct UI trees using `Html.el`, `Html.void`, and `Html.txt`. Never pass unescaped user metadata (`title`, `description`, `tags`, `hero.tagline`) to `Html.raw`.

### 3.4 Color Token Contract (`design/COLOR-THEME.md`)
When editing CSS in `lib/token_compiler.ml` or HTML class structures in `lib/site_builder.ml` / `lib/markdown.ml`:
- **Never hardcode hex/RGB colors** in component styles; always use `--sx-*` custom properties.
- **Never use `--sx-color-primary` or `--sx-color-accent` as text `color`**; they are pastel fills (`#c2e7ff`, `#ffdac2`) with low contrast on light backgrounds (`1.24:1`). Always pair them as backgrounds with `--sx-color-on-primary` / `--sx-color-on-accent` (`#1f1f1f`).
- **Use `--sx-color-link`** for un-pill'd text links (footnote numbers, TOC links).
- **Include non-color indicators** (border/underline bar, `font-weight: 600`, icon symbol, or text badge) alongside color state changes.

---

## 4. Agent Workflow & Verification Checklist

Whenever you implement a feature, fix a bug, or adjust styling:

1. **Inspect Interfaces First**: Read the relevant `lib/*.mli` files and `design/COLOR-THEME.md` (plus `LIGHTHOUSE-TASKS.md` if present locally) before editing implementation files.
2. **Implement Purely**: Make changes in `lib/` without introducing side effects or mutable state.
3. **Add Regression Tests**: Add unit or end-to-end assertions in `test/ssg_test.ml` covering both happy paths and error/security edge cases.
4. **Build & Run Tests**:
   ```bash
   dune build
   dune runtest
   ```
5. **Verify CLI Check**:
   ```bash
   dune exec subux -- check
   ```
6. **Confirm Clean Workspace**: Ensure no temporary scratch files or generated `_site/` directories are left tracked in the repository root.
