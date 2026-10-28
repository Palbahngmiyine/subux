(** Pure functional CommonMark + GFM + Obsidian + Directive Markdown processor
    built on top of vendored Cmarkit AST and Safe_url sanitizer. *)

type rendered_markdown = {
  html : string;
  toc : Domain.toc_item list;
  plain_text : string;
  has_mermaid : bool;
}

val render : string -> rendered_markdown
