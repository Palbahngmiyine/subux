(** Safe, pure functional YAML frontmatter parser.
    Enforces strict YAML security guarantees (rejecting executable tags, aliases,
    cyclic anchors, unclosed fences, and non-mapping roots) while supporting
    ambiguous colon repair and nested hero/features blocks for documentation home pages. *)

type parsed_markdown = {
  meta : Domain.frontmatter_meta;
  body : string;
}

val parse :
  ?file:string -> string -> (parsed_markdown, Domain.build_error) result
