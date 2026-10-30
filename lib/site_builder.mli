(** Pure Functional Site Graph Builder & Page Renderer for Subux SSG.
    Supports Blog_mode, Docs_mode, and Unified_mode. *)

type source_file = {
  collection : string;
  rel_path : string;
  raw_markdown : string;
}

type parsed_entry = {
  collection : string;
  rel_path : string;
  slug : Domain.Slug.t;
  title : string;
  meta : Domain.frontmatter_meta;
  rendered : Markdown.rendered_markdown;
}

type output_artifact = {
  out_path : string;
  content : string;
}

type build_plan = {
  entries : parsed_entry list;
  artifacts : output_artifact list;
}

val parse_source_file :
  source_file -> (parsed_entry, Domain.build_error) result

val sort_entries : parsed_entry list -> parsed_entry list

val group_entries_by_year :
  parsed_entry list -> (int * parsed_entry list) list

val build_series_info :
  series_name:string ->
  current_slug:Domain.Slug.t ->
  parsed_entry list ->
  Domain.series_info option

(** Renderers accept the compiled theme for inlining/fingerprinting. Without
    [stylesheet], standalone calls link the compatible external theme alias.
    Sites with custom CSS/JavaScript retain the full theme and its public tokens,
    since their dynamic selectors cannot be determined from initial HTML.
    Built-in menus expose closed/uninitialized state attributes in initial HTML
    so their dynamic rules survive the default per-page CSS optimization.
    Article, document and home titles inherit the removed Markdown H1 anchor. *)
val render_blog_home :
  ?stylesheet:string ->
  config:Domain.site_config ->
  active_tab:string ->
  parsed_entry list ->
  string

val render_blog_article :
  ?stylesheet:string ->
  config:Domain.site_config ->
  series:Domain.series_info option ->
  parsed_entry ->
  string

val render_not_found : config:Domain.site_config -> string

val render_docs_home :
  ?stylesheet:string ->
  config:Domain.site_config -> parsed_entry -> string

val render_docs_page :
  ?stylesheet:string ->
  config:Domain.site_config ->
  all_docs:parsed_entry list ->
  parsed_entry ->
  string

val compile_site :
  config:Domain.site_config ->
  tokens_json:string ->
  source_file list ->
  (build_plan, Domain.build_error) result

(** Low 32 bits of the 64-bit FNV-1a digest, as eight lowercase hex digits. *)
val content_hash : string -> string
