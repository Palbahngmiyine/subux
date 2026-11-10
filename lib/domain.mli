(** Pure Algebraic Data Types (ADTs) and Opaque Domain Types for the Subux SSG.
    All types in this module are immutable and enforce "Parse, Don't Validate". *)

type build_error =
  | Invalid_json of string
  | Invalid_token of string
  | Invalid_frontmatter of { file : string; reason : string }
  | Unsafe_yaml_construct of { file : string; detail : string }
  | Config_error of string
  | Io_error of string

val error_to_string : build_error -> string

module Result_syntax : sig
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
  val map_list : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
end

(** 1. W3C DTCG Design Token Domain *)
type mode_value =
  | Shared of string
  | Per_mode of { light : string; dark : string }

type shadow_spec = {
  offset_x : string;
  offset_y : string;
  blur : string;
  spread : string;
  color : string;
}

type token_value =
  | Color_val of mode_value
  | Dimension_val of string
  | Duration_val of string
  | Font_family_val of string list
  | Cubic_bezier_val of (string * string * string * string)
  | Shadow_val of { light : shadow_spec; dark : shadow_spec }

type design_token = {
  path : string list;
  value : token_value;
}

type area_tone =
  | Primary_tone (** --sx-color-primary (#c2e7ff) + --sx-color-on-primary (#1f1f1f) *)
  | Accent_tone  (** --sx-color-accent (#ffdac2) + --sx-color-on-accent (#1f1f1f) *)

(** 2. Validated Slug Domain *)
module Slug : sig
  type t

  val of_key : base_folder:string -> string -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_href : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

(** 3. Validated Date Domain *)
type date_val = {
  year : int;
  month : int;
  day : int;
}

val parse_date : string -> date_val option
val format_korean_full : date_val -> string
val format_korean_short : date_val -> string
val format_iso : date_val -> string
val compare_date_desc : date_val -> date_val -> int

(** 4. Obsidian Callout 16 Canonical Kinds (COLOR-THEME.md Section 6) *)
type callout_kind =
  | Callout_note
  | Callout_tip
  | Callout_important
  | Callout_warning
  | Callout_caution
  | Callout_tldr
  | Callout_info
  | Callout_todo
  | Callout_hint
  | Callout_success
  | Callout_question
  | Callout_missing
  | Callout_error
  | Callout_bug
  | Callout_example
  | Callout_cite

val callout_kind_of_string : string -> callout_kind
val callout_kind_to_string : callout_kind -> string

(** 5. Document & Site Layout Domain (Blog, Docs & Unified Modes) *)
type toc_item = {
  level : int;
  id : string;
  text : string;
}

type series_item = {
  slug : Slug.t;
  title : string;
  is_current : bool;
}

type series_info = {
  name : string;
  items : series_item list;
}

type hero_action_theme =
  | Brand_action
  | Alt_action

type hero_action = {
  theme : hero_action_theme;
  text : string;
  link : string;
}

type hero_config = {
  name : string;
  text : string option;
  tagline : string option;
  actions : hero_action list;
}

type feature_card = {
  title : string;
  details : string;
}

type layout_kind =
  | Layout_article
  | Layout_doc
  | Layout_home
  | Layout_page

type frontmatter_meta = {
  title : string option;
  description : string option;
  date : date_val option;
  series : string option;
  series_order : int option;
  layout : layout_kind option;
  order : int option;
  draft : bool;
  prev_link : (string * string) option;
  next_link : (string * string) option;
  tags : string list;
  hero : hero_config option;
  features : feature_card list;
  extra_fields : (string * string) list;
}

val empty_frontmatter : frontmatter_meta

type collection_tab = {
  id : string;
  label : string;
  folder : string;
  disclaimer : string option;
}

type nav_item = {
  text : string;
  link : string;
}

type sidebar_entry = {
  text : string;
  link : string;
}

type sidebar_group = {
  text : string;
  items : sidebar_entry list;
}

(** A validated [<link>] for icons and the web app manifest. [rel] is one of
    [icon], [apple-touch-icon], [apple-touch-icon-precomposed] or [manifest];
    [href] is a relative, [http:] or [https:] URL; optional [link_type] and
    [sizes] are printable ASCII. Values contain no control characters. *)
type head_link = private {
  rel : string;
  href : string;
  link_type : string option;
  sizes : string option;
}

val make_head_link :
  rel:string -> href:string -> link_type:string option -> sizes:string option ->
  (head_link, build_error) result
(** Reject any value outside the [head_link] contract with a [Config_error]
    that names the [headLinks] setting. *)

type site_mode =
  | Blog_mode       (** Tabbed blog & article collection layout *)
  | Docs_mode       (** Sidebar & hero documentation layout *)
  | Unified_mode    (** Hybrid blog + docs *)

type site_config = {
  title : string;
  description : string;
  lang : string;
  base_url : string;
  github_url : string option;
  mode : site_mode;
  area_tone : area_tone;
  tabs : collection_tab list;
  nav : nav_item list;
  sidebar : sidebar_group list;
  auto_sidebar : bool;
  enable_search : bool;
  custom_js : string option; (** Optional trusted module script content, loaded by the CLI. *)
  custom_css : string option; (** Optional trusted stylesheet content, loaded by the CLI. *)
  inline_css : bool; (** Inline the compiled theme in each page; defaults to true. *)
  head_links : head_link list;
      (** Emitted in every page head. Declaring an [icon] link replaces the
          default [/favicon.svg] link. *)
}

val default_blog_config : site_config
val default_docs_config : site_config
