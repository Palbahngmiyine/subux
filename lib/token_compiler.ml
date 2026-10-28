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
@import url('https://cdn.jsdelivr.net/gh/orioncactus/pretendard@v1.3.9/dist/web/variable/pretendardvariable-dynamic-subset.min.css');

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
a:where(:not(.tab-link):not(.github-link):not(.back-link):not(sup a):not(.article-link):not(.series-nav-link):not(.doc-nav-link):not(.sidebar-link):not(.toc-link):not(.hero-btn):not(.prev-next-card):not(.brand-title):not(.not-found-link)) {
  color: var(--sx-on-active-fill);
  text-decoration: none;
  background-color: var(--sx-active-fill);
  padding: 3px 8px;
  margin: 0;
  border-radius: var(--sx-radius-sm);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard);
  display: inline;
  box-decoration-break: clone;
  -webkit-box-decoration-break: clone;
}

a:where(:not(.tab-link):not(.github-link):not(.back-link):not(sup a):not(.article-link):not(.series-nav-link):not(.doc-nav-link):not(.sidebar-link):not(.toc-link):not(.hero-btn):not(.prev-next-card):not(.brand-title):not(.not-found-link)):hover {
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
  font-weight: 600;
  margin-bottom: var(--sx-space-1);
}
.callout-note { border-left-color: var(--sx-color-callout-note); }
.callout-note .callout-title { color: var(--sx-color-callout-note); }
.callout-tip { border-left-color: var(--sx-color-callout-tip); }
.callout-tip .callout-title { color: var(--sx-color-callout-tip); }
.callout-important { border-left-color: var(--sx-color-callout-important); }
.callout-important .callout-title { color: var(--sx-color-callout-important); }
.callout-warning { border-left-color: var(--sx-color-callout-warning); }
.callout-warning .callout-title { color: var(--sx-color-callout-warning); }
.callout-caution { border-left-color: var(--sx-color-callout-caution); }
.callout-caution .callout-title { color: var(--sx-color-callout-caution); }
.callout-abstract, .callout-summary, .callout-tldr { border-left-color: var(--sx-color-callout-tldr); }
.callout-abstract .callout-title, .callout-summary .callout-title, .callout-tldr .callout-title { color: var(--sx-color-callout-tldr); }
.callout-info { border-left-color: var(--sx-color-callout-info); }
.callout-info .callout-title { color: var(--sx-color-callout-info); }
.callout-todo { border-left-color: var(--sx-color-callout-todo); }
.callout-todo .callout-title { color: var(--sx-color-callout-todo); }
.callout-hint { border-left-color: var(--sx-color-callout-hint); }
.callout-hint .callout-title { color: var(--sx-color-callout-hint); }
.callout-success, .callout-check, .callout-done { border-left-color: var(--sx-color-callout-success); }
.callout-success .callout-title, .callout-check .callout-title, .callout-done .callout-title { color: var(--sx-color-callout-success); }
.callout-question, .callout-help, .callout-faq { border-left-color: var(--sx-color-callout-question); }
.callout-question .callout-title, .callout-help .callout-title, .callout-faq .callout-title { color: var(--sx-color-callout-question); }
.callout-failure, .callout-fail, .callout-missing { border-left-color: var(--sx-color-callout-missing); }
.callout-failure .callout-title, .callout-fail .callout-title, .callout-missing .callout-title { color: var(--sx-color-callout-missing); }
.callout-danger, .callout-error { border-left-color: var(--sx-color-callout-error); }
.callout-danger .callout-title, .callout-error .callout-title { color: var(--sx-color-callout-error); }
.callout-bug { border-left-color: var(--sx-color-callout-bug); }
.callout-bug .callout-title { color: var(--sx-color-callout-bug); }
.callout-example { border-left-color: var(--sx-color-callout-example); }
.callout-example .callout-title { color: var(--sx-color-callout-example); }
.callout-quote, .callout-cite { border-left-color: var(--sx-color-callout-cite); }
.callout-quote .callout-title, .callout-cite .callout-title { color: var(--sx-color-callout-cite); }

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
.doc-navbar {
  position: sticky;
  top: 0;
  z-index: 30;
  width: 100%%;
  background-color: var(--sx-color-bg-elevated);
  backdrop-filter: blur(var(--sx-blur-md));
  -webkit-backdrop-filter: blur(var(--sx-blur-md));
  border-bottom: 1px solid var(--sx-color-border);
}
.doc-navbar-inner {
  max-width: 90rem;
  margin: 0 auto;
  padding: 0 var(--sx-space-3);
  min-height: 3.75rem;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--sx-space-2);
}
a.brand-title {
  color: var(--sx-color-text);
  text-decoration: none;
  font-weight: 700;
  font-size: 1.125rem;
}
.doc-nav-links {
  display: flex;
  align-items: center;
  gap: var(--sx-space-2);
}
a.doc-nav-link {
  color: var(--sx-color-text-muted);
  text-decoration: none;
  padding: var(--sx-space-1) var(--sx-space-2);
  border-radius: var(--sx-radius-sm);
  font-weight: 500;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
a.doc-nav-link:hover, a.doc-nav-link[aria-current='page'] {
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

.doc-shell {
  max-width: 90rem;
  margin: 0 auto;
  display: grid;
  grid-template-columns: 1fr;
  gap: var(--sx-space-4);
  padding: var(--sx-space-4) var(--sx-space-3);
}
@media (min-width: 960px) {
  .doc-shell { grid-template-columns: 16rem minmax(0, 1fr); }
}
@media (min-width: 1280px) {
  .doc-shell { grid-template-columns: 16rem minmax(0, 1fr) 14rem; }
}

.doc-sidebar {
  border-right: 1px solid var(--sx-color-border);
  padding-right: var(--sx-space-2);
}
.sidebar-group { margin-bottom: var(--sx-space-3); }
.sidebar-group-title {
  font-size: 0.875rem;
  font-weight: 600;
  color: var(--sx-color-text-muted);
  margin: 0 0 var(--sx-space-1);
  padding: 0 var(--sx-space-1);
}
.sidebar-list { list-style: none; padding: 0; margin: 0; }
a.sidebar-link {
  display: flex;
  align-items: center;
  min-height: 2.25rem;
  padding: var(--sx-space-1) var(--sx-space-2);
  border-radius: var(--sx-radius-md);
  text-decoration: none;
  color: var(--sx-color-text-muted);
  font-size: 0.9375rem;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
a.sidebar-link:hover {
  background-color: var(--sx-color-surface-muted);
  color: var(--sx-color-text);
}
a.sidebar-link[aria-current='page'] {
  background-color: var(--sx-active-fill);
  color: var(--sx-on-active-fill);
  font-weight: 600;
}

.doc-toc {
  font-size: 0.875rem;
}
.doc-toc-title {
  font-weight: 600;
  color: var(--sx-color-text);
  margin: 0 0 var(--sx-space-1);
}
.doc-toc-list {
  list-style: none;
  padding: 0;
  margin: 0;
  border-left: 1px solid var(--sx-color-border);
}
a.toc-link {
  display: block;
  padding: 4px var(--sx-space-2);
  color: var(--sx-color-text-muted);
  text-decoration: none;
}
a.toc-link:hover { color: var(--sx-color-text); }
a.toc-link-h3 { padding-left: var(--sx-space-3); }

.doc-prev-next {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(14rem, 1fr));
  gap: var(--sx-space-2);
  margin-top: var(--sx-space-6);
  padding-top: var(--sx-space-3);
  border-top: 1px solid var(--sx-color-border);
}
a.prev-next-card {
  display: flex;
  flex-direction: column;
  padding: var(--sx-space-2) var(--sx-space-3);
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
  text-decoration: none;
  color: var(--sx-color-text);
  transition: background-color var(--sx-duration-fast) var(--sx-easing-standard);
}
a.prev-next-card:hover { background-color: var(--sx-color-surface-muted); }
.prev-next-label { font-size: 0.75rem; color: var(--sx-color-text-muted); }
.prev-next-title { font-weight: 600; margin-top: 4px; }

/* Hero & Features Home Layout (Documentation index.md) */
.hero-container {
  max-width: 64rem;
  margin: 0 auto;
  padding: var(--sx-space-7) var(--sx-space-3);
}
.hero-title { font-size: 2.5rem; font-weight: 700; margin: 0 0 var(--sx-space-2); }
.hero-tagline {
  font-size: 1.25rem;
  color: var(--sx-color-text-muted);
  margin: 0 0 var(--sx-space-4);
}
.hero-actions {
  display: flex;
  flex-wrap: wrap;
  gap: var(--sx-space-2);
  margin-bottom: var(--sx-space-6);
}
a.hero-btn {
  display: inline-flex;
  align-items: center;
  min-height: var(--sx-size-touch-target-min);
  padding: var(--sx-space-1) var(--sx-space-3);
  border-radius: var(--sx-radius-md);
  font-weight: 600;
  text-decoration: none;
  transition: all var(--sx-duration-fast) var(--sx-easing-standard);
}
a.hero-btn-brand {
  background-color: var(--sx-active-fill);
  color: var(--sx-on-active-fill);
}
a.hero-btn-brand:hover { background-color: var(--sx-active-fill-hover); }
a.hero-btn-alt {
  background-color: var(--sx-color-chip-neutral);
  color: var(--sx-color-on-chip-neutral);
}

.features-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(18rem, 1fr));
  gap: var(--sx-space-3);
}
.feature-card {
  background-color: var(--sx-color-surface-muted);
  border: 1px solid var(--sx-color-border);
  border-radius: var(--sx-radius-md);
  padding: var(--sx-space-3);
}
.feature-card h2 {
  margin: 0 0 var(--sx-space-1);
  font-size: 1.125rem;
  font-weight: 600;
}
.feature-card p {
  margin: 0;
  color: var(--sx-color-text-muted);
  font-size: 0.9375rem;
}

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

let compile_full_stylesheet ?(prefix = "sx") ?(area_tone = Primary_tone)
    json_str =
  let+ tokens = parse_tokens json_str in
  let vars_css = compile_variables_css ~prefix tokens in
  vars_css ^ component_css_template area_tone
