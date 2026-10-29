(** Compiler for W3C DTCG Design Tokens (tokens.json -> theme.css).
    Implements COLOR-THEME.md Section 0.1-0.4 and Section 8-9. *)

val parse_tokens :
  string -> (Domain.design_token list, Domain.build_error) result

val token_var_name : ?prefix:string -> string list -> string

val compile_variables_css :
  ?prefix:string -> Domain.design_token list -> string

(** Remove comments and redundant spacing, preserving strings, escapes,
    descendant selectors and whitespace required by CSS values. *)
val minify_css : string -> string

(** Minify generated Subux CSS, fold repeated theme colors with [light-dark()],
    and retain custom properties reachable from component rules. The external
    stylesheet retains every token and the media-query compatibility fallback.
    Inline output targets browsers supporting CSS Color 5 [light-dark()]. *)
val inline_stylesheet : string -> string

(** Retain only rules needed by the rendered page and the built-in search,
    Mermaid and interactive states, then optimize its inline stylesheet. *)
val critical_stylesheet : html:string -> string -> string

val compile_full_stylesheet :
  ?prefix:string ->
  ?area_tone:Domain.area_tone ->
  string ->
  (string, Domain.build_error) result
