(** Compiler for W3C DTCG Design Tokens (tokens.json -> theme.css).
    Implements COLOR-THEME.md Section 0.1-0.4 and Section 8-9. *)

val parse_tokens :
  string -> (Domain.design_token list, Domain.build_error) result

val token_var_name : ?prefix:string -> string list -> string

val compile_variables_css :
  ?prefix:string -> Domain.design_token list -> string

val compile_full_stylesheet :
  ?prefix:string ->
  ?area_tone:Domain.area_tone ->
  string ->
  (string, Domain.build_error) result
