(** Pure functional CommonMark + GFM + Obsidian + Directive Markdown processor
    built on top of vendored Cmarkit AST and Safe_url sanitizer. *)

type rendered_markdown = {
  html : string;
  toc : Domain.toc_item list;
  plain_text : string;
  has_mermaid : bool;
}

(** Code fences expose a stable wrapper, language label and copy control for
    site themes. Each footnote reference has a matching backlink in the note's
    final paragraph; repeated backlinks carry their reference number. References
    point to an accessible footnote heading. GFM table code spans remove the
    table delimiter escape before pipes; code outside tables preserves it.
    All source text and language names remain HTML-escaped. *)
val render : string -> rendered_markdown
