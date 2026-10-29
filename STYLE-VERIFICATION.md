# Style verification

## Scope and evidence

The visual target is zero differing RGB pixels, without masks or color tolerance,
at 1280 × 900 and 390 × 900 throughout each page.

The source-profile validation on 2026-10-29 covered these unchanged checkouts:

| Original | Revision | Renderer |
| --- | --- | --- |
| [just-tips](https://github.com/Palbahngmiyine/just-tips) | `7b50d08ff8187ae8e509ed0fd47748a844ec0c27` | VitePress 1.6.4 |
| [subux.dev](https://github.com/Palbahngmiyine/subux.dev) | `e0c22d96287a25365e846f11a6da073393c24c71` | Original Qwik/Vite app |

The final comparison on the original Subux base `3d72f11` passed:

| Check | Result |
| --- | --- |
| 17 routes, complete viewport scroll coverage | 246/246 identical: 108 desktop, 138 mobile |
| All docs pages, light and dark, both widths | 20/20 identical |
| Open mobile navigation/sidebar, light and dark | 4/4 identical |
| Open mobile outline at scroll 0, 32 and 750, light and dark | 6/6 identical |
| OS-dark home/document viewport smoke checks | 6/6 identical |

The complete controlled run passed all 246 cases. After the last mobile CSS fix,
docs pages were recaptured; the final manifest combines their 10 light captures
with 236 blog captures whose generated files were unchanged. Asset SHA-256
snapshots confirmed that reuse. This is a local fixed-source comparison, not
proof of the current public deployment. Generated sites, PNGs, manifests and
temporary source clones are ignored local evidence rather than committed assets.
The preview and reference servers were stopped after verification.

## Browser conditions and limits

The controlled baseline uses installed headed Chrome 156.0.8078.49 with
`--disable-gpu-rasterization`, `--force-color-profile=srgb` and
`--blink-settings=imageAnimationPolicy=2`. The runner asserts the applied flags
and software rasterizer. Original and generated pages use identical viewport,
theme and reduced-motion options.

Default-renderer diagnostics passed 235/246, with small image/link paint
differences. Original-source self-captures also varied: 73 link-corner pixels on
desktop and 532 image pixels on mobile. Eleven image response bodies matched
byte-for-byte. Controlling rasterization and animated-image timing made all 36
repeated original-dev/original-production/Subux diagnostic comparisons identical.
This does not establish pixel identity across other browsers, GPU drivers,
operating systems, fonts or animation times.

`browser_image_policy.js` is a positive control: the original five-frame GIF
changes after 3.5 seconds in normal serving and stays still only under the
capture policy, on both sites. Production profiles retain normal GIF animation.

The runner captures actual viewports with 150px overlap, checks dimensions and
scroll position before/after each capture, and rejects gaps or unexpected hover.
Chrome's full-page capture removed classic scrollbars and changed the rendered
width in this environment, so it is not used as an acceptance gate.
Native video controls remain visible. Playback at 16× followed by a completed
seek to zero and a continuous-full-buffer assertion aligns their initial frame
and download-progress bar without masking pixels or replacing source styles.

Chromium documents the animation setting in
[web_preferences.mojom](https://chromium.googlesource.com/chromium/src.git/+/c15545a7333c8b2705aa9b9587e2dbf08454e9ae/third_party/blink/public/mojom/webpreferences/web_preferences.mojom)
and [settings.json5](https://chromium.googlesource.com/chromium/src/+/721f48ecb704833c9fd1a3414d57ef48c7a969af/third_party/blink/renderer/core/frame/settings.json5).
Its [GPU test arguments](https://chromium.googlesource.com/chromium/src/+/master/content/test/gpu/gpu_tests/common_browser_args.py)
include rasterization and color-profile controls.

## Reproduce

Apply the profiles and build both original inputs using
[examples/README.md](examples/README.md). The browser helpers expect local servers:

| Port | Content |
| --- | --- |
| 4173 | Generated just-tips |
| 4174 | Generated subux.dev |
| 4175 | Original just-tips VitePress app |
| 4176 | Search-enabled default docs fixture |
| 4177 | Original subux.dev Qwik/Vite app |

Use the upstream projects' installed toolchains to run their original apps.
The search fixture uses the same just-tips Markdown with `enableSearch: true`
and without profile CSS/JS. Subux itself gains no npm/opam runtime dependency;
Playwright, Chrome and Pillow belong only to the verification environment.
Both opt-in profiles load public CDN fonts/icons/highlighters.

Invoke browser helper function expressions with Playwright's `browser_run_code`
file runner. Each helper owns fresh contexts. For `browser_scrolled.js`, set the
parent viewport to each target width and invoke with
`{ controlled: true, captureTag: 'verify' }`. Concatenate both returned arrays
into `output/scroll-verify-manifest.json`. Optional `sites` and `colorScheme`
arguments support scoped docs/dark checks. Progress in
`window.__subuxScrollRuns[width]` survives a tool-response timeout; inspect its
status before restarting a run. Shut down the test servers when finished.

```sh
dune build
dune runtest
dune exec subux -- check
python3 test/compare_scrolled.py --full output/scroll-verify-manifest.json
python3 test/compare_pixels.py --dark
python3 test/compare_pixels.py --outline
python3 test/compare_pixels.py --controls
```

Run `browser_theme.js`, `browser_outline.js` and `browser_controls.js` before
their matching comparisons. Comparators reject missing captures, invalid
viewport dimensions and any differing RGB pixel. The complete scroll gate also
requires all 17 routes at both widths, consistent renderer settings and gap-free
coverage. `browser_layout.js` and `browser_interactions.js` cover overflow,
search/focus restoration, theme persistence, Escape/backdrop dismissal,
sidebar focus containment, clipboard contents and copy-feedback reset.
`browser_controls.js` exercises actual wheel input to verify background scroll
locking and restoration.

Native regressions cover unsafe YAML/URLs, escaping, table code-span pipes,
repeated footnote backlinks and separate CSS/JS artifacts in every mode.
The CLI tests use real files and a bounded child process to verify regular files,
internal/external symlinks, traversal, absolute paths, invalid types and FIFOs.
