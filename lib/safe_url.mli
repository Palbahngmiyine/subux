(** Pure URL sanitization and WikiLink normalization following WHATWG URL rules
    and strict scheme allowlist security specifications. *)

val is_safe_url :
  allow_contact:bool -> decode_entities:bool -> string -> bool

val sanitize_url :
  allow_contact:bool -> decode_entities:bool -> string -> string option

val wiki_link_href : string -> string
