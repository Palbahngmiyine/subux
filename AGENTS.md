# Instructions

## Read First

- Read the relevant source and its `.mli` before writing tests or changing behavior. Treat the code as the source of truth, not names, docs, or memory.
- Check the current checkout, Git status, and affected callers before editing. Preserve user-owned changes.
- Keep this file aligned with the source. Describe proposed architecture as a proposal until implemented.

## Project Invariants

- **No runtime dependencies.** Subux builds with only the OCaml compiler and Dune. Do not add PPX, C stubs, or external opam libraries; the CommonMark parser stays vendored in `vendor/cmarkit/`. Browser and pixel-comparison tooling under `test/` is test-only and must not become a build or runtime requirement.
- **Pure library, imperative shell.** `lib/` performs no filesystem, network, environment, or clock access and keeps no global mutable state. All I/O stays in `bin/main.ml`.
- **No exceptions in `lib/`.** Report failures as `result` values carrying `Domain.build_error`.
- **Contracts live in `.mli`.** Change the interface and its doc comment together with the implementation; keep helpers private by leaving them out of the interface.
- **Warnings are errors.** Fix the warning; do not relax the dune flags.

## Security

- Frontmatter rejects YAML tags, anchors, and aliases.
- Every URL emitted into `href`, `src`, or `::video{src=...}` passes through `Safe_url`. Only `http:`, `https:`, and relative URLs are allowed, plus `mailto:` and `tel:` for links. Leading and trailing control characters and spaces, and embedded tabs and newlines, are removed before the scheme is read.
- Build HTML with the escaping constructors in `Html`. Never pass user content or metadata (titles, descriptions, tags, hero text) to `Html.raw`.
- Add a test for every new input path, including the hostile case it must reject.

## Color Contract

`design/tokens.json` is the source of truth; `design/COLOR-THEME.md` summarizes it.

- Component styles use `--sx-*` custom properties, never hardcoded colors.
- Primary and accent are fills only. Put text on them with `--sx-color-on-primary` / `--sx-color-on-accent`; use `--sx-color-link` for text links without a fill.
- Pair every color state change with a non-color indicator such as a border, underline, weight, or icon.

## Testing

- Add a regression for every bug fix and show that it fails on the unfixed code. Cover the error and security edge cases, not only the happy path.
- Derive expected results from the contract, not from the implementation under test. Never weaken a check to get a passing result.
- Before handing off, run `dune build`, `dune runtest`, and `dune exec subux -- check`. For visible output changes, also run the affected browser checks in `test/` and report which ones did not run.
- Leave no generated sites or scratch files tracked in the repository.

## Pull Request Evidence

Every pull request description includes an artifact that settles what it claims, so a reviewer can confirm the change without reproducing it.

- Behavior change or bug fix: the regression's output failing on the base revision and passing on the head.
- Rendered output or styling: before and after screenshots, or the pixel-comparison result.
- Documentation only: the consistency checks run and their results.
- Bind each artifact to the head commit and the environment that produced it. When an artifact cannot be produced, name the claim that stays unverified and why.
