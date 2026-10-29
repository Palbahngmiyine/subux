# Site profiles

These profiles represent each original site's layout, fonts, palette and client
behavior using Subux configuration and optional site-owned CSS/JavaScript.

| Original | Input directory | Configuration | CSS | JavaScript |
| --- | --- | --- | --- | --- |
| just-tips | Repository root | `just-tips.json` | `just-tips.css` | `just-tips.js` |
| subux.dev | `src/` | `subux-dev.json` | `subux-dev.css` | `subux-dev.js` |

In the selected input directory, save the corresponding JSON as `subux.json`,
CSS as `site.css` and JavaScript as `site.js`. Merge configuration into an existing
`subux.json` rather than replacing unrelated settings. Set `baseUrl` to the site's
public URL when preparing a published build.

Run these commands from the Subux checkout, substituting your input paths:

```sh
dune exec subux -- build --mode docs -i /path/to/just-tips -o /tmp/just-tips-site
python3 -m http.server 4173 --bind 127.0.0.1 --directory /tmp/just-tips-site
```

For the blog, use a separate terminal:

```sh
dune exec subux -- build --mode blog -i /path/to/subux.dev/src -o /tmp/subux-dev-site
python3 -m http.server 4174 --bind 127.0.0.1 --directory /tmp/subux-dev-site
```

Open <http://127.0.0.1:4173/> or <http://127.0.0.1:4174/>.

`customCss` and `customJs` name files relative to the input directory. Subux emits
their contents as `assets/custom.css` and `assets/custom.js`; the custom stylesheet
follows the built-in theme and JavaScript loads as an external module. Paths must
stay inside the input directory, including after resolving symlinks. JavaScript
executes as trusted site code. Resource URLs inside CSS retain their original
values, so referenced resources must be available at those URLs.

OCaml callers constructing `Domain.site_config` directly must add `custom_css`
and `custom_js` (`string option`, normally `None`). These fields contain trusted
file contents, not paths; the CLI performs file loading outside the pure library.
Callers using the supplied defaults inherit `None` for both fields.

Keep site-specific color and font changes in the CSS profile's `--sx-*` custom
properties. Navigation, sidebar and search settings belong in `subux.json`.
Both examples disable search to match their originals; the default is enabled.
The just-tips theme follows system preferences and the theme switch.
The subux.dev profile uses the original site's light theme in both OS modes.

Both profiles load fonts/icons and optional syntax highlighters from public CDNs.
If highlighting fails, escaped native code remains readable; exact visual
verification reports the failure. Subux itself gains no npm/opam runtime
dependency. See [the verification record](../STYLE-VERIFICATION.md) for the
original revisions, browser conditions and current comparison results.

Sites with custom CSS/JavaScript retain the full theme and public tokens, including
styles for dynamically added classes. The default site keeps per-page critical CSS
optimization. `inlineCss: false` selects the fingerprinted external theme instead.
