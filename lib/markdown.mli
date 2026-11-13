(** Pure functional CommonMark + GFM + Obsidian + Directive Markdown processor
    built on top of vendored Cmarkit AST and Safe_url sanitizer. *)

type rendered_markdown = {
  html : string;
  page_title_id : string option; (** Unique anchor of the removed leading H1, for the template title. *)
  toc : Domain.toc_item list;
  plain_text : string;
  has_mermaid : bool;
}

(** Code fences expose a stable wrapper, language label and copy control for
    site themes. Each footnote reference has a matching backlink in the note's
    final paragraph; repeated backlinks carry their reference number. References
    point to an accessible footnote heading. GFM table code spans remove the
    table delimiter escape before pipes without repeating whitespace
    normalization; code outside tables preserves it.
    All source text and language names remain HTML-escaped.

    Code in a fence carries highlight.js token classes ([hljs-*]). YAML marks
    document markers, comments, list bullets, keys, literals, numbers and
    scalars. A shell reads URLs, paths, numbers and dotted names as plain words
    and opens a comment with [#] only where a word starts. In other languages
    [//] opens a comment unless it follows a colon, and never in Python. JSON
    punctuation is a token of its own, and a JSON literal nests a keyword
    token, so a theme styles both as it does highlight.js output.

    Without [heading_base], preserve the source heading levels. Site templates
    use [heading_base:2] beneath their own page title, which also normalizes
    forward level jumps. [page_title] removes only a matching leading H1 that
    the template already renders, including a Setext heading. Its unique ID is
    retained in [page_title_id] and reserved against subsequent body headings.
    Heading anchors also avoid template navigation and outline control IDs.
    Other body content and later headings are preserved.

    Images are deferred with [loading="lazy"] and [decoding="async"] unless
    [lazy_images] is false. *)
val render :
  ?heading_base:int -> ?page_title:string -> ?lazy_images:bool -> string -> rendered_markdown
