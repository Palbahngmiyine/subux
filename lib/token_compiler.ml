open Domain
open Domain.Result_syntax

let find_field key fields = List.assoc_opt key fields

let format_num f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Printf.sprintf "%g" f

let parse_shadow_spec fields =
  match
    ( find_field "offsetX" fields,
      find_field "offsetY" fields,
      find_field "blur" fields,
      find_field "spread" fields,
      find_field "color" fields )
  with
  | ( Some (Json_parser.String offset_x),
      Some (Json_parser.String offset_y),
      Some (Json_parser.String blur),
      Some (Json_parser.String spread),
      Some (Json_parser.String color) ) ->
      Ok { offset_x; offset_y; blur; spread; color }
  | _ -> Error (Invalid_token "Malformed shadow spec object")

let parse_token_value path_Rev inherited_type raw_val =
  let path_str = String.concat "." (List.rev path_Rev) in
  match inherited_type with
  | Some "color" -> (
      match raw_val with
      | Json_parser.String s -> Ok (Color_val (Shared s))
      | Json_parser.Object fields -> (
          match (find_field "light" fields, find_field "dark" fields) with
          | Some (Json_parser.String light), Some (Json_parser.String dark) ->
              Ok (Color_val (Per_mode { light; dark }))
          | _ ->
              Error
                (Invalid_token
                   ("Color token must have string or {light, dark}: " ^ path_str))
          )
      | _ -> Error (Invalid_token ("Invalid color token value at " ^ path_str)))
  | Some "dimension" -> (
      match raw_val with
      | Json_parser.String s -> Ok (Dimension_val s)
      | _ -> Error (Invalid_token ("Dimension token must be string: " ^ path_str)))
  | Some "duration" -> (
      match raw_val with
      | Json_parser.String s -> Ok (Duration_val s)
      | _ -> Error (Invalid_token ("Duration token must be string: " ^ path_str)))
  | Some "fontFamily" -> (
      match raw_val with
      | Json_parser.Array items ->
          let* families =
            Result_syntax.map_list
              (function
                | Json_parser.String s -> Ok s
                | _ ->
                    Error
                      (Invalid_token ("Font family item must be string: " ^ path_str)))
              items
          in
          Ok (Font_family_val families)
      | _ -> Error (Invalid_token ("fontFamily must be array: " ^ path_str)))
  | Some "cubicBezier" -> (
      match raw_val with
      | Json_parser.Array
          [
            Json_parser.Number a;
            Json_parser.Number b;
            Json_parser.Number c;
            Json_parser.Number d;
          ] ->
          Ok
            (Cubic_bezier_val
               (format_num a, format_num b, format_num c, format_num d))
      | _ ->
          Error
            (Invalid_token ("cubicBezier must be 4-number array: " ^ path_str)))
  | Some "shadow" -> (
      match raw_val with
      | Json_parser.Object fields -> (
          match (find_field "light" fields, find_field "dark" fields) with
          | Some (Json_parser.Object l_fields), Some (Json_parser.Object d_fields)
            ->
              let* light = parse_shadow_spec l_fields in
              let* dark = parse_shadow_spec d_fields in
              Ok (Shadow_val { light; dark })
          | _ ->
              Error
                (Invalid_token ("Shadow token requires light/dark at " ^ path_str))
          )
      | _ -> Error (Invalid_token ("Shadow token must be object at " ^ path_str)))
  | Some other ->
      Error (Invalid_token ("Unsupported $type '" ^ other ^ "' at " ^ path_str))
  | None -> Error (Invalid_token ("Missing inherited $type at " ^ path_str))

let rec collect_tokens path_rev inherited_type fields =
  let current_type =
    match find_field "$type" fields with
    | Some (Json_parser.String t) -> Some t
    | _ -> inherited_type
  in
  match find_field "$value" fields with
  | Some raw_val ->
      let+ tv = parse_token_value path_rev current_type raw_val in
      [ { path = List.rev path_rev; value = tv } ]
  | None ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (k, v) :: rest ->
            if String.length k > 0 && k.[0] = '$' then loop acc rest
            else if path_rev = [] && k = "palette" then
              (* Section 0.1 & 0.3: palette.* is never emitted to CSS *)
              loop acc rest
            else (
              match v with
              | Json_parser.Object sub_fields ->
                  let* sub_tokens =
                    collect_tokens (k :: path_rev) current_type sub_fields
                  in
                  loop (List.rev_append sub_tokens acc) rest
              | _ -> loop acc rest)
      in
      loop [] fields

let parse_tokens json_str =
  let* root = Json_parser.parse json_str in
  match root with
  | Json_parser.Object fields -> collect_tokens [] None fields
  | _ -> Error (Invalid_token "Root of tokens.json must be a JSON object")

let token_var_name ?(prefix = "sx") path =
  "--" ^ prefix ^ "-" ^ String.concat "-" path

let format_font_family names =
  let quote_if_space name =
    if String.contains name ' ' then "\"" ^ name ^ "\"" else name
  in
  String.concat ", " (List.map quote_if_space names)

let format_shadow (s : shadow_spec) =
  if s.spread = "0" || s.spread = "0px" then
    Printf.sprintf "%s %s %s %s" s.offset_x s.offset_y s.blur s.color
  else
    Printf.sprintf "%s %s %s %s %s" s.offset_x s.offset_y s.blur s.spread
      s.color

let light_css_value = function
  | Color_val (Shared s) -> s
  | Color_val (Per_mode { light; _ }) -> light
  | Dimension_val s -> s
  | Duration_val s -> s
  | Font_family_val names -> format_font_family names
  | Cubic_bezier_val (a, b, c, d) ->
      Printf.sprintf "cubic-bezier(%s, %s, %s, %s)" a b c d
  | Shadow_val { light; _ } -> format_shadow light

let dark_css_value_opt = function
  | Color_val (Shared _)
  | Dimension_val _
  | Duration_val _
  | Font_family_val _
  | Cubic_bezier_val _ ->
      None
  | Color_val (Per_mode { dark; _ }) -> Some dark
  | Shadow_val { dark; _ } -> Some (format_shadow dark)

let compile_variables_css ?(prefix = "sx") tokens =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf ":root {\n  color-scheme: light dark;\n";
  List.iter
    (fun tok ->
      let var_name = token_var_name ~prefix tok.path in
      let value_str = light_css_value tok.value in
      Buffer.add_string buf (Printf.sprintf "  %s: %s;\n" var_name value_str))
    tokens;
  Buffer.add_string buf "}\n\n";
  let write_dark_declarations indent =
    List.iter
      (fun tok ->
        match dark_css_value_opt tok.value with
        | Some dark_val ->
            let var_name = token_var_name ~prefix tok.path in
            Buffer.add_string buf
              (Printf.sprintf "%s%s: %s;\n" indent var_name dark_val)
        | None -> ())
      tokens
  in
  Buffer.add_string buf
    "@media (prefers-color-scheme: dark) {\n  :root:not([data-theme=\"light\"]) {\n";
  write_dark_declarations "    ";
  Buffer.add_string buf "  }\n}\n\n";
  Buffer.add_string buf ":root[data-theme=\"dark\"] {\n";
  write_dark_declarations "  ";
  Buffer.add_string buf "}\n";
  Buffer.contents buf

let component_css_template area_tone =
  let active_fill, active_hover, on_active =
    match area_tone with
    | Primary_tone ->
        ( "var(--sx-color-primary)",
          "var(--sx-color-primary-hover)",
          "var(--sx-color-on-primary)" )
    | Accent_tone ->
        ( "var(--sx-color-accent)",
          "var(--sx-color-accent-hover)",
          "var(--sx-color-on-accent)" )
  in
  Printf.sprintf
    {|
/* Pretendard v1.3.9 hhea: 1950/-494/0 at 2048 UPM.
   Full-width Korean fallback uses 100%% size-adjust. */
@font-face {
  font-family: "Subux Gothic Fallback";
  src: local("Apple SD Gothic Neo"), local("Malgun Gothic");
  size-adjust: 100%%;
  ascent-override: 95.21484375%%;
  descent-override: 24.12109375%%;
  line-gap-override: 0%%;
}

:root {
  --sx-active-fill: %s;
  --sx-active-fill-hover: %s;
  --sx-on-active-fill: %s;

  /* Backwards-compatible aliases for standard theme variables */
  --space-1: var(--sx-space-1);
  --space-2: var(--sx-space-2);
  --space-3: var(--sx-space-3);
  --space-4: var(--sx-space-4);
  --space-5: var(--sx-space-5);
  --space-6: var(--sx-space-6);
  --space-7: var(--sx-space-7);
  --space-8: var(--sx-space-8);
  --space-9: var(--sx-space-9);
  --space-10: var(--sx-space-10);
  --color-primary: var(--sx-active-fill);
  --color-primary-hover: var(--sx-active-fill-hover);
  --color-text: var(--sx-color-text);
  --color-text-muted: var(--sx-color-text-muted);
  --color-bg: var(--sx-color-bg);
  --color-bg-elevated: var(--sx-color-bg-elevated);
  --color-selection-bg: var(--sx-color-selection-bg);
  --color-selection-text: var(--sx-color-selection-text);
  --color-border: var(--sx-color-border);
  --color-code-bg: var(--sx-color-surface-muted);
  --font-sans: var(--sx-font-sans);
  --font-mono: var(--sx-font-mono);
  --radius-sm: var(--sx-radius-sm);
  --radius-md: var(--sx-radius-md);
  --radius-lg: var(--sx-radius-lg);
  --radius-xl: var(--sx-radius-xl);
  --blur-sm: var(--sx-blur-sm);
  --blur-md: var(--sx-blur-md);
  --blur-lg: var(--sx-blur-lg);
  --shadow-sm: var(--sx-shadow-sm);
  --shadow-md: var(--sx-shadow-md);
  --shadow-lg: var(--sx-shadow-lg);
  --transition-fast: var(--sx-duration-fast) var(--sx-easing-standard);
  --transition-normal: var(--sx-duration-normal) var(--sx-easing-standard);
  --transition-slow: var(--sx-duration-slow) var(--sx-easing-standard);
  --touch-target-min: var(--sx-size-touch-target-min);
}

*, *::before, *::after {
  box-sizing: border-box;
  font-family: var(--sx-font-sans);
}

::selection {
  background-color: var(--sx-color-selection-bg);
  color: var(--sx-color-selection-text);
}

html, body {
  margin: 0;
  padding: 0;
  background-color: var(--sx-color-bg);
  color: var(--sx-color-text);
  overflow-x: hidden;
  max-width: 100vw;
  font-size: 1rem;
  line-height: 1.625;
  word-break: keep-all;
  overflow-wrap: break-word;
}

@media (min-width: 1024px) {
  html, body { font-size: 1.0625rem; }
}

:focus-visible {
  outline: 2px solid var(--sx-color-focus-ring);
  outline-offset: 2px;
}

/* Typography Hierarchy (COLOR-THEME.md Section 7: h1/h2 weight 400, h3 weight 600) */
h1 {
  font-size: 1.75rem;
  font-weight: 400;
  margin: var(--sx-space-2) 0;
  color: var(--sx-color-text);
  line-height: 1.25;
}
@media (min-width: 640px) { h1 { font-size: 2rem; } }
@media (min-width: 1024px) { h1 { font-size: 2.25rem; } }

h2 {
  font-size: 1.375rem;
  font-weight: 400;
  margin: var(--sx-space-4) 0 var(--sx-space-2);
  color: var(--sx-color-text);
  line-height: 1.33;
}
@media (min-width: 640px) { h2 { font-size: 1.5rem; } }

h3 {
  font-size: 1.125rem;
  font-weight: 600;
  margin: var(--sx-space-3) 0 var(--sx-space-1);
  color: var(--sx-color-text);
  line-height: 1.4;
}
@media (min-width: 640px) { h3 { font-size: 1.25rem; } }

/* Rule 2 & 3: Inline links are pastel pills (fill + on-primary text) */
.article-content a:where(:not(sup a)) {
  color: var(--sx-on-active-fill);
  text-decoration: underline;
  text-underline-offset: .15em;
  background-color: var(--sx-active-fill);
  padding: 3px 8px;
  margin: 0;
  border-radius: var(--sx-radius-sm);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard);
  display: inline;
  box-decoration-break: clone;
  -webkit-box-decoration-break: clone;
}

.article-content a:where(:not(sup a)):hover {
  background-color: var(--sx-active-fill-hover);
}

/* Blog & Article Collection Layout */
.site-container {
  width: 100%%;
  max-width: 100%%;
  margin: 0 auto;
  padding: var(--sx-space-4) var(--sx-space-2);
  display: flex;
  flex-direction: column;
  align-items: center;
}
@media (min-width: 640px) { .site-container { padding: var(--sx-space-5) var(--sx-space-4); } }
@media (min-width: 1024px) { .site-container { padding: var(--sx-space-6) var(--sx-space-6); } }

.page-wrapper, .content-wrapper {
  width: 100%%;
  max-width: 45rem;
  margin: 0 auto;
}
@media (min-width: 1024px) {
  .page-wrapper, .content-wrapper { max-width: min(60rem, 100%% - 12rem); }
}

.site-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: var(--sx-space-5);
  padding-bottom: var(--sx-space-3);
}
.site-header h1 {
  margin: 0;
  font-size: 1.5rem;
  font-weight: 700;
  letter-spacing: -0.02em;
}
@media (min-width: 640px) { .site-header h1 { font-size: 1.75rem; } }

.header-actions {
  display: flex;
  align-items: center;
  gap: var(--sx-space-1);
}

a.github-link, button.icon-btn {
  background: none;
  border: none;
  cursor: pointer;
  padding: var(--sx-space-1);
  color: var(--sx-color-text-muted);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard), color var(--sx-duration-fast) var(--sx-easing-standard);
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: var(--sx-size-touch-target-min);
  min-height: var(--sx-size-touch-target-min);
  border-radius: var(--sx-radius-md);
}
a.github-link:hover, button.icon-btn:hover {
  background: var(--sx-color-surface-muted);
  color: var(--sx-color-text);
}

.tabs-container { width: 100%%; }
.tabs-nav {
  display: flex;
  gap: var(--sx-space-1);
  margin-bottom: var(--sx-space-4);
  border-bottom: 1px solid var(--sx-color-border);
  width: 100%%;
}
a.tab-link {
  background: none;
  text-decoration: none;
  padding: var(--sx-space-2) var(--sx-space-1);
  margin: 0 var(--sx-space-3) 0 0;
  color: var(--sx-color-text-muted);
  border-bottom: 2px solid transparent;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
  min-height: var(--sx-size-touch-target-min);
  display: inline-flex;
  align-items: center;
  font-size: 1rem;
  font-weight: 500;
}
a.tab-link:hover { color: var(--sx-color-text); }
a.tab-link[aria-current='page'] {
  color: var(--sx-color-text);
  border-bottom-color: var(--sx-active-fill);
  font-weight: 600;
}

.disclaimer-banner {
  padding: var(--sx-space-2) var(--sx-space-3);
  margin-bottom: var(--sx-space-3);
  background-color: var(--sx-color-surface-muted);
  border-left: 3px solid var(--sx-color-text-muted);
  border-radius: 0 var(--sx-radius-sm) var(--sx-radius-sm) 0;
  font-size: 0.875rem;
  color: var(--sx-color-text-muted);
}
.disclaimer-banner p { margin: 0; color: var(--sx-color-text-muted); }

.articles { margin-top: var(--sx-space-4); width: 100%%; }
.articles h2 {
  font-size: 1.125rem;
  font-weight: 400;
  color: var(--sx-color-text-muted);
  margin: var(--sx-space-5) 0 var(--sx-space-2);
}
.articles h2:first-child { margin-top: 0; }
.articles ul { list-style: none; padding: 0; margin: 0; }
.articles li {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: var(--sx-space-2);
  min-height: var(--sx-size-touch-target-min);
  padding: var(--sx-space-1) 0;
  margin: var(--sx-space-1) 0;
}
.articles li time {
  color: var(--sx-color-text-muted);
  font-size: 0.9375rem;
  flex-shrink: 0;
  min-width: 5.5rem;
}
.articles li a.article-link {
  background-color: var(--sx-active-fill);
  color: var(--sx-on-active-fill);
  padding: 8px 14px;
  display: inline-flex;
  align-items: center;
  min-height: calc(var(--sx-size-touch-target-min) - var(--sx-space-2));
  border-radius: var(--sx-radius-md);
  text-decoration: none;
  font-weight: 500;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
.articles li a.article-link:hover {
  background-color: var(--sx-active-fill-hover);
  box-shadow: var(--sx-shadow-sm);
}

a.back-link {
  color: var(--sx-on-active-fill);
  text-decoration: none;
  background-color: var(--sx-active-fill);
  padding: var(--sx-space-1) var(--sx-space-2);
  font-size: 0.9375rem;
  font-weight: 500;
  border-radius: var(--sx-radius-md);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard);
  display: inline-flex;
  align-items: center;
  min-height: var(--sx-size-touch-target-min);
}
a.back-link:hover { background-color: var(--sx-active-fill-hover); }

.article-nav { margin-bottom: var(--sx-space-5); }
.article-header { margin-bottom: var(--sx-space-5); }
.article-header h1 { margin: 0 0 var(--sx-space-1); }
.article-header time { color: var(--sx-color-text-muted); font-size: 0.9375rem; }
.article-content { margin-top: var(--sx-space-4); max-width: 70ch; }
.article-content p, .article-content ul, .article-content ol, .article-content blockquote {
  margin-bottom: var(--sx-space-3);
}
.article-content .article-video {
  display: block;
  width: 100%%;
  margin: var(--sx-space-4) 0;
  border-radius: var(--sx-radius-md);
  background: var(--sx-color-bg);
  box-shadow: var(--sx-shadow-sm);
}

/* Series Navigation Box */
.series-nav-box {
  max-width: 70ch;
  margin: 0 0 var(--sx-space-4);
  padding: var(--sx-space-2) var(--sx-space-3);
  background-color: var(--sx-color-surface-muted);
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
}
.series-nav-label {
  margin: 0 0 var(--sx-space-1);
  font-size: 0.875rem;
  font-weight: 600;
  color: var(--sx-color-text-muted);
}
.series-nav-list { margin: 0; padding-left: var(--sx-space-4); }
.series-nav-list li {
  margin: var(--sx-space-1) 0;
  font-size: 0.9375rem;
  color: var(--sx-color-text-muted);
}
.series-nav-current { font-weight: 600; color: var(--sx-color-text); }
a.series-nav-link {
  color: var(--sx-color-text);
  background-color: transparent;
  text-decoration: underline;
  text-decoration-color: var(--sx-active-fill);
  text-decoration-thickness: 2px;
  text-underline-offset: 3px;
}
a.series-nav-link:hover { text-decoration-color: var(--sx-active-fill-hover); }

/* Blockquotes & Obsidian Callouts (COLOR-THEME.md Section 6: 16 callout tokens) */
hr { border: none; border-top: 1px solid var(--sx-color-border); margin: var(--sx-space-4) 0; }
blockquote {
  margin: var(--sx-space-3) 0;
  padding-left: var(--sx-space-3);
  border-left: 3px solid var(--sx-color-border);
  font-style: italic;
  color: var(--sx-color-text-muted);
}
blockquote strong { font-style: normal; }

[data-callout] {
  border-left: 3px solid var(--sx-color-border);
  padding: var(--sx-space-2) var(--sx-space-3);
  margin: var(--sx-space-3) 0;
  background-color: var(--sx-color-surface-muted);
  border-radius: 0 var(--sx-radius-md) var(--sx-radius-md) 0;
  font-style: normal;
  color: var(--sx-color-text);
}
[data-callout] .callout-title {
  color: var(--sx-color-text);
  font-weight: 600;
  margin-bottom: var(--sx-space-1);
}
.callout-note { border-left-color: var(--sx-color-callout-note); }
.callout-tip { border-left-color: var(--sx-color-callout-tip); }
.callout-important { border-left-color: var(--sx-color-callout-important); }
.callout-warning { border-left-color: var(--sx-color-callout-warning); }
.callout-caution { border-left-color: var(--sx-color-callout-caution); }
.callout-abstract, .callout-summary, .callout-tldr { border-left-color: var(--sx-color-callout-tldr); }
.callout-info { border-left-color: var(--sx-color-callout-info); }
.callout-todo { border-left-color: var(--sx-color-callout-todo); }
.callout-hint { border-left-color: var(--sx-color-callout-hint); }
.callout-success, .callout-check, .callout-done { border-left-color: var(--sx-color-callout-success); }
.callout-question, .callout-help, .callout-faq { border-left-color: var(--sx-color-callout-question); }
.callout-failure, .callout-fail, .callout-missing { border-left-color: var(--sx-color-callout-missing); }
.callout-danger, .callout-error { border-left-color: var(--sx-color-callout-error); }
.callout-bug { border-left-color: var(--sx-color-callout-bug); }
.callout-example { border-left-color: var(--sx-color-callout-example); }
.callout-quote, .callout-cite { border-left-color: var(--sx-color-callout-cite); }

/* Tables, Code & Syntax Highlighting (COLOR-THEME.md Section 5) */
.table-wrapper {
  overflow-x: auto;
  margin: var(--sx-space-3) 0;
  border-radius: var(--sx-radius-md);
  border: 1px solid var(--sx-color-border);
}
table { width: 100%%; border-collapse: collapse; margin: 0; }
th {
  background-color: var(--sx-color-surface-muted);
  border-bottom: 1px solid var(--sx-color-border);
  padding: var(--sx-space-2);
  font-weight: 600;
  text-align: left;
}
th:not(:last-child), td:not(:last-child) { border-right: 1px solid var(--sx-color-border); }
td { border-bottom: 1px solid var(--sx-color-border); padding: var(--sx-space-2); }
tr:last-child td { border-bottom: none; }

pre {
  background-color: var(--sx-color-surface-muted);
  padding: var(--sx-space-2) var(--sx-space-3);
  margin: var(--sx-space-3) 0;
  overflow-x: auto;
  font-size: 0.875rem;
  line-height: 1.625;
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
}
code {
  background-color: var(--sx-color-surface-muted);
  padding: 2px 6px;
  font-size: 0.875rem;
  font-family: var(--sx-font-mono);
  border-radius: var(--sx-radius-sm);
}
pre code {
  background: none;
  padding: 0;
  color: var(--sx-color-code-text);
}
.hljs-comment, .hljs-quote { color: var(--sx-color-code-comment); font-style: italic; }
.hljs-keyword, .hljs-selector-tag, .hljs-subst { color: var(--sx-color-code-keyword); }
.hljs-number, .hljs-literal, .hljs-variable, .hljs-attr, .hljs-attribute, .hljs-meta, .hljs-symbol { color: var(--sx-color-code-number); }
.hljs-string, .hljs-doctag, .hljs-regexp { color: var(--sx-color-code-string); }
.hljs-title, .hljs-section, .hljs-selector-id { color: var(--sx-color-code-title); font-weight: 600; }
.hljs-type { color: var(--sx-color-code-type); }
.hljs-name, .hljs-tag { color: var(--sx-color-code-tag); }
.hljs-built_in, .hljs-bullet, .hljs-code { color: var(--sx-color-code-builtin); }
.hljs-deletion { color: var(--sx-color-code-deletion-fg); background-color: var(--sx-color-code-deletion-bg); }
.hljs-addition { color: var(--sx-color-code-addition-fg); background-color: var(--sx-color-code-addition-bg); }

.article-content .mermaid {
  display: block;
  width: 100%%;
  margin: var(--sx-space-4) 0;
  padding: var(--sx-space-3);
  overflow-x: auto;
  background: var(--sx-color-code-diagram-bg);
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
}

/* Footnotes & Hover Tooltips (Rule 3: text-only link uses --sx-color-link) */
.footnotes {
  margin-top: var(--sx-space-6);
  padding-top: var(--sx-space-3);
  border-top: 1px solid var(--sx-color-border);
  font-size: 0.9375rem;
}
sup a {
  background: none;
  padding: 0;
  color: var(--sx-color-link);
  font-weight: 600;
  text-decoration: underline;
  text-underline-offset: 2px;
}
sup a:hover { color: var(--sx-color-focus-ring); }
sup[data-footnote-content] { position: relative; cursor: help; }
sup[data-footnote-content]::after {
  content: attr(data-footnote-content);
  position: absolute;
  bottom: calc(100%% + 10px);
  left: 50%%;
  transform: translateX(-50%%);
  background: var(--sx-color-text);
  color: var(--sx-color-bg);
  padding: var(--sx-space-1) var(--sx-space-2);
  border-radius: var(--sx-radius-sm);
  font-size: 0.8125rem;
  font-weight: 400;
  line-height: 1.5;
  white-space: normal;
  width: max-content;
  max-width: 300px;
  pointer-events: none;
  opacity: 0;
  transition: opacity var(--sx-duration-fast) var(--sx-easing-standard);
  z-index: 10;
  box-shadow: var(--sx-shadow-md);
}
sup[data-footnote-content]:hover::after { opacity: 1; }

/* Documentation Mode Layout (VitePress-compatible using Color tokens) */
.jt-navbar {
  position: sticky;
  top: 0;
  z-index: 30;
  width: 100%%;
  background-color: var(--sx-color-bg-elevated);
  backdrop-filter: blur(var(--sx-blur-md));
  -webkit-backdrop-filter: blur(var(--sx-blur-md));
  border-bottom: 1px solid var(--sx-color-border);
}
.jt-navbar-inner {
  max-width: 90rem;
  margin: 0 auto;
  padding: 0 var(--sx-space-3);
  min-height: 3.75rem;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--sx-space-2);
}
a.jt-brand {
  color: var(--sx-color-text);
  text-decoration: none;
  font-weight: 700;
  font-size: 1.125rem;
}
.jt-nav-links {
  display: flex;
  align-items: center;
  gap: var(--sx-space-2);
}
a.jt-nav-link {
  color: var(--sx-color-text-muted);
  text-decoration: none;
  padding: var(--sx-space-1) var(--sx-space-2);
  border-radius: var(--sx-radius-sm);
  font-weight: 500;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
a.jt-nav-link:hover, a.jt-nav-link[aria-current='page'] {
  color: var(--sx-color-text);
  background-color: var(--sx-color-surface-muted);
}
.keycap-pill {
  background-color: var(--sx-active-fill);
  color: var(--sx-on-active-fill);
  border-radius: var(--sx-radius-sm);
  padding: 2px 8px;
  font-size: 0.75rem;
  font-weight: 700;
  font-family: var(--sx-font-mono);
}

.jt-doc-layout {
  max-width: 90rem;
  margin: 0 auto;
  display: grid;
  grid-template-columns: 1fr;
  gap: var(--sx-space-4);
  padding: var(--sx-space-4) var(--sx-space-3);
}
@media (min-width: 960px) {
  .jt-doc-layout { grid-template-columns: 16rem minmax(0, 1fr); }
}
@media (min-width: 1280px) {
  .jt-doc-layout { grid-template-columns: 16rem minmax(0, 1fr) 14rem; }
}

.jt-sidebar {
  border-right: 1px solid var(--sx-color-border);
  padding-right: var(--sx-space-2);
}
.jt-sidebar-group { margin-bottom: var(--sx-space-3); }
.jt-sidebar-title {
  font-size: 0.875rem;
  font-weight: 600;
  color: var(--sx-color-text-muted);
  margin: 0 0 var(--sx-space-1);
  padding: 0 var(--sx-space-1);
}
.jt-sidebar-list { list-style: none; padding: 0; margin: 0; }
a.jt-sidebar-link {
  display: flex;
  align-items: center;
  min-height: var(--sx-size-touch-target-min);
  padding: var(--sx-space-1) var(--sx-space-2);
  border-radius: var(--sx-radius-md);
  text-decoration: none;
  color: var(--sx-color-text-muted);
  font-size: 0.9375rem;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
a.jt-sidebar-link:hover {
  background-color: var(--sx-color-surface-muted);
  color: var(--sx-color-text);
}
a.jt-sidebar-link[aria-current='page'] {
  background-color: var(--sx-active-fill);
  color: var(--sx-on-active-fill);
  font-weight: 600;
}

.jt-outline {
  font-size: 0.875rem;
}
.jt-outline-title {
  font-weight: 600;
  color: var(--sx-color-text);
  margin: 0 0 var(--sx-space-1);
}
.jt-outline ul {
  list-style: none;
  padding: 0;
  margin: 0;
  border-left: 1px solid var(--sx-color-border);
}
.jt-outline a {
  display: block;
  padding: 4px var(--sx-space-2);
  color: var(--sx-color-text-muted);
  text-decoration: none;
}
.jt-outline a:hover { color: var(--sx-color-text); }
.jt-outline a.toc-link-h3 { padding-left: var(--sx-space-3); }

.jt-doc-footer {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(14rem, 1fr));
  gap: var(--sx-space-2);
  margin-top: var(--sx-space-6);
  padding-top: var(--sx-space-3);
  border-top: 1px solid var(--sx-color-border);
}
a.jt-pager-link {
  display: flex;
  flex-direction: column;
  padding: var(--sx-space-2) var(--sx-space-3);
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
  text-decoration: none;
  color: var(--sx-color-text);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard);
}
a.jt-pager-link:hover { background-color: var(--sx-color-surface-muted); }
.jt-pager-label { font-size: 0.75rem; color: var(--sx-color-text-muted); }
.jt-pager-title { font-weight: 600; margin-top: 4px; }

/* Hero & Features Home Layout (Documentation index.md) */
.jt-hero {
  max-width: 64rem;
  margin: 0 auto;
  padding: var(--sx-space-7) var(--sx-space-3);
}
.jt-hero-name { font-size: 2.5rem; font-weight: 700; margin: 0 0 var(--sx-space-2); }
.jt-hero-tagline {
  font-size: 1.25rem;
  color: var(--sx-color-text-muted);
  margin: 0 0 var(--sx-space-4);
}
.jt-hero-actions {
  display: flex;
  flex-wrap: wrap;
  gap: var(--sx-space-2);
  margin-bottom: var(--sx-space-6);
}
a.jt-btn {
  display: inline-flex;
  align-items: center;
  min-height: var(--sx-size-touch-target-min);
  padding: var(--sx-space-1) var(--sx-space-3);
  border-radius: var(--sx-radius-md);
  font-weight: 600;
  text-decoration: none;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
a.jt-btn-brand {
  background-color: var(--sx-active-fill);
  color: var(--sx-on-active-fill);
}
a.jt-btn-brand:hover { background-color: var(--sx-active-fill-hover); }
a.jt-btn-alt {
  background-color: var(--sx-color-chip-neutral);
  color: var(--sx-color-on-chip-neutral);
}

.jt-features {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(min(100%%, 18rem), 1fr));
  gap: var(--sx-space-3);
}
.jt-feature-card {
  background-color: var(--sx-color-surface-muted);
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
  padding: var(--sx-space-3);
}
.jt-feature-card h2 {
  margin: 0 0 var(--sx-space-1);
  font-size: 1.125rem;
  font-weight: 600;
}
.jt-feature-card p {
  margin: 0;
  color: var(--sx-color-text-muted);
  font-size: 0.9375rem;
}

/* Shared accessibility, media, and documentation controls. */
.skip-link {
  position: fixed; top: var(--sx-space-1); left: var(--sx-space-1); z-index: 100;
  transform: translateY(-200%%); padding: var(--sx-space-2);
  background: var(--sx-active-fill); color: var(--sx-on-active-fill);
  border-radius: var(--sx-radius-md);
}
.skip-link:focus-visible { transform: translateY(0); }
[hidden], .jt-search-modal[hidden] { display: none !important; }
.article-content img { max-width: 100%%; height: auto; border-radius: var(--sx-radius-md); display: block; }
.article-content .article-video { aspect-ratio: 16 / 9; }
[data-callout][data-expandable="true"] .callout-title {
  cursor: pointer; user-select: none; display: flex; align-items: center; justify-content: space-between;
}
[data-callout][data-expandable="true"] .callout-title::after {
  content: "▾"; transition: transform var(--sx-duration-fast) var(--sx-easing-standard);
}
details[data-callout]:not([open]) .callout-title::after { transform: rotate(-90deg); }
details[data-callout]:not([open]) .callout-content { display: none; }
.jt-doc-main { min-width: 0; }
.jt-outline a { color: var(--sx-color-link); min-height: var(--sx-size-touch-target-min); }
.jt-hero-text { font-size: 1.5rem; }
.jt-features { max-width: 80rem; margin: 0 auto; padding: var(--sx-space-3); }
.jt-brand, .jt-nav-link, .jt-search-btn, .jt-theme-toggle {
  display: inline-flex; align-items: center; min-height: var(--sx-size-touch-target-min);
}
.jt-search-btn, .jt-theme-toggle {
  border: 1px solid var(--sx-color-border); border-radius: var(--sx-radius-sm);
  padding: var(--sx-space-1) var(--sx-space-2); cursor: pointer;
  color: var(--sx-color-text); background: var(--sx-color-surface-muted);
}
.jt-theme-toggle { min-width: var(--sx-size-touch-target-min); justify-content: center; }
.jt-search-modal {
  position: fixed; inset: 0; z-index: 50; padding: var(--sx-space-3);
  display: flex; justify-content: center; align-items: flex-start;
  background: var(--sx-color-surface-muted); backdrop-filter: blur(var(--sx-blur-sm));
  overflow-y: auto;
}
.jt-search-dialog {
  width: min(100%%, 40rem); margin-top: 10vh; padding: var(--sx-space-3);
  background: var(--sx-color-bg); color: var(--sx-color-text);
  border: 1px solid var(--sx-color-border); border-radius: var(--sx-radius-lg); box-shadow: var(--sx-shadow-lg);
}
.jt-search-input {
  width: 100%%; min-height: var(--sx-size-touch-target-min); font-size: 1rem;
  background: var(--sx-color-bg); color: var(--sx-color-text);
  border: 1px solid var(--sx-color-border); border-radius: var(--sx-radius-sm); padding: var(--sx-space-1);
}
@media (max-width: 959px) {
  .jt-outline { display: none; }
  .jt-navbar-inner, .jt-nav-links { flex-wrap: wrap; }
  .jt-doc-layout { gap: var(--sx-space-2); padding: var(--sx-space-2); }
}
@media (min-width: 960px) and (max-width: 1279px) { .jt-outline { display: none; } }
@media (prefers-reduced-motion: reduce) { *, *::before, *::after { transition: none !important; } }

/* 404 Not Found & Empty State */
.empty-state {
  text-align: center;
  padding: var(--sx-space-8) var(--sx-space-2);
  color: var(--sx-color-text-muted);
}
.not-found-page {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  text-align: center;
  min-height: 50vh;
  padding: var(--sx-space-6) var(--sx-space-4);
}
.not-found-title {
  font-size: 2rem;
  font-weight: 600;
  color: var(--sx-color-text);
  margin: 0 0 var(--sx-space-4);
}
a.not-found-link {
  display: inline-flex;
  align-items: center;
  padding: var(--sx-space-2) var(--sx-space-4);
  color: var(--sx-on-active-fill);
  background-color: var(--sx-active-fill);
  text-decoration: none;
  font-weight: 500;
  border-radius: var(--sx-radius-md);
  min-height: var(--sx-size-touch-target-min);
}
a.not-found-link:hover { background-color: var(--sx-active-fill-hover); }

.sr-only {
  position: absolute;
  width: 1px;
  height: 1px;
  padding: 0;
  margin: -1px;
  overflow: hidden;
  clip: rect(0, 0, 0, 0);
  white-space: nowrap;
  border: 0;
}
|}
    active_fill active_hover on_active

let minify_css css =
  let len = String.length css in
  let buf = Buffer.create len in
  let space = function ' ' | '\t' | '\r' | '\n' | '\012' -> true | _ -> false in
  let tight = function '{' | '}' | ';' | ',' -> true | _ -> false in
  let rec comment i =
    if i + 1 >= len then len
    else if css.[i] = '*' && css.[i + 1] = '/' then i + 2
    else comment (i + 1)
  in
  let rec scan i quote pending previous =
    if i >= len then Buffer.contents buf
    else
      let c = css.[i] in
      match quote with
      | Some q ->
          Buffer.add_char buf c;
          if c = '\\' && i + 1 < len then begin
            Buffer.add_char buf css.[i + 1];
            scan (i + 2) quote false (Some css.[i + 1])
          end else scan (i + 1) (if c = q then None else quote) false (Some c)
      | None ->
          if i + 4 <= len && String.lowercase_ascii (String.sub css i 4) = "url(" then begin
            if pending && (match previous with None -> false | Some p -> not (tight p || p = ':'))
            then Buffer.add_char buf ' ';
            let rec url_end j quote =
              if j >= len then len
              else if css.[j] = '\\' then url_end (min len (j + 2)) quote
              else match quote with
              | Some q -> url_end (j + 1) (if css.[j] = q then None else quote)
              | None -> if css.[j] = ')' then j + 1
                  else url_end (j + 1) (if css.[j] = '\'' || css.[j] = '"' then Some css.[j] else None)
            in
            let j = url_end (i + 4) None in
            Buffer.add_substring buf css i (j - i);
            scan j None false (Some ')')
          end else if c = '/' && i + 1 < len && css.[i + 1] = '*' then
            scan (comment (i + 2)) None pending previous
          else if space c then scan (i + 1) None true previous
          else begin
            if pending && not (tight c)
               && (match previous with None -> false | Some p -> not (tight p || p = ':'))
            then Buffer.add_char buf ' ';
            Buffer.add_char buf c;
            if c = '\\' && i + 1 < len then begin
              Buffer.add_char buf css.[i + 1];
              scan (i + 2) None false (Some css.[i + 1])
            end else scan (i + 1) (if c = '\'' || c = '"' then Some c else None) false (Some c)
          end
  in
  scan 0 None false None

module Name_set = Set.Make (String)

(* The public theme retains the documented media-query form. The inline
   bundle can fold the two identical dark declaration lists into light-dark(). *)
let compact_color_modes css =
  let rec take_until marker acc = function
    | [] -> None
    | line :: rest when line = marker -> Some (List.rev acc, rest)
    | line :: rest -> take_until marker (line :: acc) rest
  in
  let declaration line =
    let text = String.trim line in
    match String.index_opt text ':' with
    | Some i when String.starts_with ~prefix:"--" text && String.ends_with ~suffix:";" text ->
        Some (String.sub text 0 i,
          String.trim (String.sub text (i + 1) (String.length text - i - 2)))
    | Some _ | None -> None
  in
  match String.split_on_char '\n' css with
  | ":root {" :: rest ->
      begin match take_until "}" [] rest with
      | Some (light, "" :: "@media (prefers-color-scheme: dark) {" :: "  :root:not([data-theme=\"light\"]) {" :: rest) ->
          begin match take_until "  }" [] rest with
          | Some (dark, "}" :: "" :: ":root[data-theme=\"dark\"] {" :: rest) ->
              begin match take_until "}" [] rest with
              | Some (explicit_dark, tail) when List.map String.trim dark = List.map String.trim explicit_dark ->
                  let dark = List.filter_map declaration dark in
                  let combine value dark_value =
                    if String.starts_with ~prefix:"#" value && String.starts_with ~prefix:"#" dark_value then
                      Some ("light-dark(" ^ value ^ "," ^ dark_value ^ ")")
                    else
                      match String.rindex_opt value ' ', String.rindex_opt dark_value ' ' with
                      | Some i, Some j when String.sub value 0 i = String.sub dark_value 0 j ->
                          let c = String.sub value (i + 1) (String.length value - i - 1) in
                          let d = String.sub dark_value (j + 1) (String.length dark_value - j - 1) in
                          if String.starts_with ~prefix:"#" c && String.starts_with ~prefix:"#" d then
                            Some (String.sub value 0 (i + 1) ^ "light-dark(" ^ c ^ "," ^ d ^ ")")
                          else None
                      | _ -> None
                  in
                  let converted = List.map (fun line -> match declaration line with
                    | Some (name, value) ->
                        begin match List.assoc_opt name dark with
                        | None -> Some line
                        | Some d -> Option.map (fun v -> "  " ^ name ^ ": " ^ v ^ ";") (combine value d)
                        end
                    | None -> Some line) light in
                  if List.for_all Option.is_some converted then
                    String.concat "\n" ([":root {"] @ List.filter_map Fun.id converted @
                      ["}"; ":root[data-theme=\"light\"] { color-scheme: light; }";
                       ":root[data-theme=\"dark\"] { color-scheme: dark; }"] @ tail)
                  else css
              | _ -> css
              end
          | _ -> css
          end
      | _ -> css
      end
  | _ -> css

let alias_component_values css =
  let len = String.length css in
  let rec scan i quote counts =
    if i >= len then counts
    else match quote with
    | Some q ->
        if css.[i] = '\\' then scan (min len (i + 2)) quote counts
        else scan (i + 1) (if css.[i] = q then None else quote) counts
    | None ->
        if css.[i] = '\'' || css.[i] = '"' then scan (i + 1) (Some css.[i]) counts
        else if i + 9 < len && String.sub css i 9 = "var(--sx-" then
          begin match String.index_from_opt css (i + 9) ')' with
          | Some j ->
              let name = String.sub css (i + 4) (j - i - 4) in
              let n = Option.value ~default:0 (List.assoc_opt name counts) in
              scan (j + 1) None ((name, n + 1) :: List.remove_assoc name counts)
          | None -> scan (i + 1) None counts
          end
        else scan (i + 1) None counts
  in
  let counts = scan 0 None [] |> List.sort (fun (a, ac) (b, bc) ->
    let by_count = Int.compare bc ac in if by_count = 0 then String.compare a b else by_count) in
  let rec short_name n =
    let last = String.make 1 (Char.chr (Char.code 'a' + n mod 26)) in
    if n < 26 then last else short_name (n / 26 - 1) ^ last
  in
  let has_declaration name =
    let needle = name ^ ":" in
    let rec scan i = i + String.length needle <= len &&
      (String.sub css i (String.length needle) = needle || scan (i + 1)) in scan 0
  in
  let aliases = List.mapi (fun i (name, count) ->
    let alias = "--sx-" ^ short_name i in
    if (String.length name - String.length alias) * count > String.length name + String.length alias + 8
       && not (has_declaration alias) && not (String.contains name ',') then Some (name, alias) else None) counts
    |> List.filter_map Fun.id in
  let buf = Buffer.create len in
  let rec write i quote =
    if i >= len then ()
    else match quote with
    | Some q ->
        Buffer.add_char buf css.[i];
        if css.[i] = '\\' && i + 1 < len then begin
          Buffer.add_char buf css.[i + 1]; write (i + 2) quote
        end else write (i + 1) (if css.[i] = q then None else quote)
    | None ->
        if css.[i] = '\'' || css.[i] = '"' then begin
          Buffer.add_char buf css.[i]; write (i + 1) (Some css.[i])
        end else if i + 9 < len && String.sub css i 9 = "var(--sx-" then
          begin match String.index_from_opt css (i + 9) ')' with
          | Some j ->
              let name = String.sub css (i + 4) (j - i - 4) in
              Buffer.add_string buf ("var(" ^ Option.value ~default:name (List.assoc_opt name aliases) ^ ")");
              write (j + 1) None
          | None -> Buffer.add_char buf css.[i]; write (i + 1) None
          end
        else begin Buffer.add_char buf css.[i]; write (i + 1) None end
  in
  write 0 None;
  (* Appended aliases retain the public token values and their theme cascade. *)
  Buffer.add_string buf ":root{";
  List.iter (fun (name, alias) -> Buffer.add_string buf (alias ^ ":var(" ^ name ^ ");")) aliases;
  Buffer.add_char buf '}';
  Buffer.contents buf

let inline_stylesheet css =
  let css = compact_color_modes css in
  let references text =
    let len = String.length text in
    let rec scan i names =
      if i + 6 > len then names
      else if String.sub text i 6 = "var(--" then
        let rec finish j = if j < len &&
          (match text.[j] with 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true | _ -> false)
          then finish (j + 1) else j in
        let j = finish (i + 4) in
        scan j (Name_set.add (String.sub text (i + 4) (j - i - 4)) names)
      else scan (i + 1) names
    in scan 0 Name_set.empty
  in
  let declaration line =
    let text = String.trim line in
    if String.starts_with ~prefix:"--" text then
      match String.index_opt text ':' with
      | Some i -> Some (String.sub text 0 i)
      | None -> None
    else None
  in
  let lines = String.split_on_char '\n' css in
  let roots = List.fold_left (fun names line -> match declaration line with
    | Some _ -> names | None -> Name_set.union names (references line)) Name_set.empty lines in
  let rec reachable names =
    let expanded = List.fold_left (fun acc line -> match declaration line with
      | Some name when Name_set.mem name names -> Name_set.union acc (references line)
      | Some _ | None -> acc) names lines in
    if Name_set.equal names expanded then names else reachable expanded
  in
  let used = reachable roots in
  lines |> List.filter (fun line -> match declaration line with
    | Some name -> Name_set.mem name used | None -> true)
  |> String.concat "\n" |> minify_css |> alias_component_values

let critical_stylesheet ~html css =
  let contains text part =
    let n = String.length part in
    let rec scan i = i + n <= String.length text &&
      (String.sub text i n = part || scan (i + 1)) in
    scan 0
  in
  let ident = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true | _ -> false in
  let classes =
    let rec scan i names =
      if i + 7 > String.length html then names
      else if String.sub html i 7 = "class=\"" then
        match String.index_from_opt html (i + 7) '"' with
        | None -> names
        | Some j ->
            let names = String.sub html (i + 7) (j - i - 7) |> String.split_on_char ' '
              |> List.fold_left (fun names name -> Name_set.add name names) names in
            scan (j + 1) names
      else scan (i + 1) names
    in scan 0 (List.fold_left (fun s c -> Name_set.add c s) Name_set.empty
      ["jt-sidebar-link"; "mermaid"])
  in
  let selector_used selector =
    let n = String.length selector in
    let rec skip_parens i depth =
      if i >= n then n else match selector.[i] with
      | '(' -> skip_parens (i + 1) (depth + 1)
      | ')' -> if depth = 1 then i + 1 else skip_parens (i + 1) (depth - 1)
      | _ -> skip_parens (i + 1) depth
    in
    let rec scan i =
      if i >= n then true
      else if selector.[i] = ':' then
        let rec finish j = if j < n && ident selector.[j] then finish (j + 1) else j in
        let j = finish (i + 1) in
        if j < n && selector.[j] = '(' then scan (skip_parens j 0) else scan j
      else if selector.[i] = '.' then
        let rec finish j = if j < n && ident selector.[j] then finish (j + 1) else j in
        let j = finish (i + 1) in
        Name_set.mem (String.sub selector (i + 1) (j - i - 1)) classes && scan j
      else if selector.[i] = '[' then
        begin match String.index_from_opt selector (i + 1) ']' with
        | None -> true
        | Some j ->
            let attr = String.sub selector (i + 1) (j - i - 1) in
            let name = match String.index_opt attr '=' with Some k -> String.sub attr 0 k | None -> attr in
            (name = "hidden" || name = "data-theme" || contains html (name ^ "=") || contains html (name ^ ">")) && scan (j + 1)
        end
      else if (selector.[i] >= 'a' && selector.[i] <= 'z') &&
              (i = 0 || selector.[i - 1] = ' ' || selector.[i - 1] = '>') then
        let rec finish j = if j < n && ident selector.[j] then finish (j + 1) else j in
        let j = finish (i + 1) in
        let tag = String.sub selector i (j - i) in
        (tag = "html" || tag = "body" || contains html ("<" ^ tag ^ " ") || contains html ("<" ^ tag ^ ">")) && scan j
      else scan (i + 1)
    in scan 0
  in
  let css = minify_css (compact_color_modes css) in
  let len = String.length css in
  let rec closing i depth quote =
    if i >= len then len
    else match quote with
    | Some q -> if css.[i] = '\\' then closing (i + 2) depth quote
        else closing (i + 1) depth (if css.[i] = q then None else quote)
    | None -> match css.[i] with
      | '\'' | '"' as q -> closing (i + 1) depth (Some q)
      | '{' -> closing (i + 1) (depth + 1) None
      | '}' -> if depth = 1 then i else closing (i + 1) (depth - 1) None
      | _ -> closing (i + 1) depth None
  in
  let rec rules start finish =
    if start >= finish then ""
    else match String.index_from_opt css start '{' with
    | None -> String.sub css start (finish - start)
    | Some open_at when open_at < finish ->
        let close_at = closing (open_at + 1) 1 None in
        if close_at >= finish then String.sub css start (finish - start)
        else
          let selector = String.sub css start (open_at - start) in
          let media = String.starts_with ~prefix:"@media" selector in
          let keep = media || String.starts_with ~prefix:"@font-face" selector ||
            List.exists selector_used (String.split_on_char ',' selector) in
          let body = if not keep then "" else if media then rules (open_at + 1) close_at
            else String.sub css (open_at + 1) (close_at - open_at - 1) in
          let current = if keep && body <> "" then selector ^ "{" ^ body ^ "}" else "" in
          current ^ rules (close_at + 1) finish
    | Some _ -> String.sub css start (finish - start)
  in
  (* Filter the original formatted sheet before variable reachability and theme
     folding. A fresh line per declaration preserves the inline compiler input. *)
  let filtered = rules 0 len in
  let formatted = Buffer.create (String.length filtered) in
  let rec format i quote =
    if i >= String.length filtered then () else
    let c = filtered.[i] in
    Buffer.add_char formatted c;
    match quote with
    | Some q ->
        if c = '\\' && i + 1 < String.length filtered then begin
          Buffer.add_char formatted filtered.[i + 1]; format (i + 2) quote
        end else format (i + 1) (if c = q then None else quote)
    | None ->
        if c = '{' || c = '}' || c = ';' then Buffer.add_char formatted '\n';
        format (i + 1) (if c = '\'' || c = '"' then Some c else None)
  in
  format 0 None;
  (* Color-mode folding uses the original generated prelude, so first fold it
     before rule selection and retain the already-folded formatted declarations. *)
  inline_stylesheet (Buffer.contents formatted)

let compile_full_stylesheet ?(prefix = "sx") ?(area_tone = Primary_tone)
    json_str =
  let+ tokens = parse_tokens json_str in
  let vars_css = compile_variables_css ~prefix tokens in
  vars_css ^ component_css_template area_tone
