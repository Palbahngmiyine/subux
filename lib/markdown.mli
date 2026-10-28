(** Pure functional CommonMark + GFM + Obsidian + Directive Markdown processor
    built on top of vendored Cmarkit AST and Safe_url sanitizer. *)

type rendered_markdown = {
  html : string;
  toc : Domain.toc_item list;
  plain_text : string;
  has_mermaid : bool;
}

(** [heading_base] is the minimum heading level (default 1). Site templates
    use 2 beneath their own page title. Forward level jumps are normalized. *)
val render : ?heading_base:int -> string -> rendered_markdown
