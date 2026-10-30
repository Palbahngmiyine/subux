(** Pure functional CommonMark + GFM + Obsidian + Directive Markdown processor
    built on top of vendored Cmarkit AST and Safe_url sanitizer. *)

type rendered_markdown = {
  html : string;
  toc : Domain.toc_item list;
  plain_text : string;
  has_mermaid : bool;
}

(** Without [heading_base], preserve the source heading levels. Site templates
    use [heading_base:2] beneath their own page title, which also normalizes
    forward level jumps. *)
val render : ?heading_base:int -> string -> rendered_markdown
