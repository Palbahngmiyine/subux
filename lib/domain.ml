type build_error =
  | Invalid_json of string
  | Invalid_token of string
  | Invalid_frontmatter of { file : string; reason : string }
  | Unsafe_yaml_construct of { file : string; detail : string }
  | Config_error of string
  | Io_error of string

let error_to_string = function
  | Invalid_json msg -> "Invalid JSON: " ^ msg
  | Invalid_token msg -> "Invalid Design Token: " ^ msg
  | Invalid_frontmatter { file; reason } ->
      "Invalid frontmatter in " ^ file ^ ": " ^ reason
  | Unsafe_yaml_construct { file; detail } ->
      "Unsafe YAML construct in " ^ file ^ ": " ^ detail
  | Config_error msg -> "Configuration error: " ^ msg
  | Io_error msg -> "I/O error: " ^ msg

module Result_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r

  let map_list f xs =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | x :: rest ->
          let* y = f x in
          loop (y :: acc) rest
    in
    loop [] xs
end

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
  | Primary_tone
  | Accent_tone

module Slug = struct
  type t = string

  let index_candidates = [ "@index.md"; "@index.mdx"; "index.md"; "index.mdx" ]

  let normalize_slashes s =
    String.map (fun c -> if c = '\\' then '/' else c) s

  let strip_leading_slashes s =
    let len = String.length s in
    let rec find i =
      if i < len && s.[i] = '/' then find (i + 1)
      else String.sub s i (len - i)
    in
    find 0

  let strip_md_ext s =
    let lower = String.lowercase_ascii s in
    if String.ends_with ~suffix:".mdx" lower then
      String.sub s 0 (String.length s - 4)
    else if String.ends_with ~suffix:".md" lower then
      String.sub s 0 (String.length s - 3)
    else s

  let of_key ~base_folder key =
    let norm = normalize_slashes key in
    let prefix = base_folder ^ "/" in
    let after_base =
      let plen = String.length prefix in
      let nlen = String.length norm in
      let rec find_sub i =
        if i + plen > nlen then norm
        else if String.sub norm i plen = prefix then
          String.sub norm (i + plen) (nlen - i - plen)
        else find_sub (i + 1)
      in
      if base_folder = "" then strip_leading_slashes norm else find_sub 0
    in
    let is_index =
      List.exists
        (fun cand ->
          after_base = cand || String.ends_with ~suffix:("/" ^ cand) after_base)
        index_candidates
    in
    if is_index then
      match List.rev (String.split_on_char '/' after_base) with
      | _ :: rev_dirs -> String.concat "/" (List.rev rev_dirs)
      | [] -> ""
    else strip_md_ext (strip_leading_slashes after_base)

  let of_string s = strip_md_ext (strip_leading_slashes (normalize_slashes s))
  let to_string s = s
  let to_href s = if s = "" || s = "index" then "/" else "/" ^ s
  let equal = String.equal
  let compare = String.compare
end

type date_val = {
  year : int;
  month : int;
  day : int;
}

let parse_date raw =
  let s = String.trim raw in
  let date_part =
    match String.split_on_char 'T' s with
    | d :: _ -> (
        match String.split_on_char ' ' d with d2 :: _ -> d2 | [] -> d)
    | [] -> s
  in
  match String.split_on_char '-' date_part with
  | [ y_s; m_s; d_s ] -> (
      match (int_of_string_opt y_s, int_of_string_opt m_s, int_of_string_opt d_s) with
      | Some year, Some month, Some day
        when year >= 1900 && year <= 9999 && month >= 1 && month <= 12
             && day >= 1 && day <= 31 ->
          Some { year; month; day }
      | _ -> None)
  | _ -> None

let format_korean_full d =
  Printf.sprintf "%d년 %d월 %d일" d.year d.month d.day

let format_korean_short d = Printf.sprintf "%d월 %d일" d.month d.day

let format_iso d = Printf.sprintf "%04d-%02d-%02d" d.year d.month d.day

let compare_date_desc a b =
  let cy = Int.compare b.year a.year in
  if cy <> 0 then cy
  else
    let cm = Int.compare b.month a.month in
    if cm <> 0 then cm else Int.compare b.day a.day

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

let callout_kind_of_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "note" -> Callout_note
  | "abstract" | "summary" | "tldr" -> Callout_tldr
  | "info" -> Callout_info
  | "todo" -> Callout_todo
  | "tip" -> Callout_tip
  | "hint" -> Callout_hint
  | "important" -> Callout_important
  | "success" | "check" | "done" -> Callout_success
  | "question" | "help" | "faq" -> Callout_question
  | "warning" | "attention" -> Callout_warning
  | "caution" -> Callout_caution
  | "failure" | "missing" | "fail" -> Callout_missing
  | "danger" | "error" -> Callout_error
  | "bug" -> Callout_bug
  | "example" -> Callout_example
  | "quote" | "cite" -> Callout_cite
  | _ -> Callout_note

let callout_kind_to_string = function
  | Callout_note -> "note"
  | Callout_tip -> "tip"
  | Callout_important -> "important"
  | Callout_warning -> "warning"
  | Callout_caution -> "caution"
  | Callout_tldr -> "tldr"
  | Callout_info -> "info"
  | Callout_todo -> "todo"
  | Callout_hint -> "hint"
  | Callout_success -> "success"
  | Callout_question -> "question"
  | Callout_missing -> "missing"
  | Callout_error -> "error"
  | Callout_bug -> "bug"
  | Callout_example -> "example"
  | Callout_cite -> "cite"

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

let empty_frontmatter =
  {
    title = None;
    description = None;
    date = None;
    series = None;
    series_order = None;
    layout = None;
    order = None;
    draft = false;
    prev_link = None;
    next_link = None;
    tags = [];
    hero = None;
    features = [];
    extra_fields = [];
  }

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

type site_mode =
  | Blog_mode
  | Docs_mode
  | Unified_mode

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
  custom_js : string option;
  custom_css : string option;

  inline_css : bool;
}

let default_blog_config =
  {
    title = "Subux Blog";
    description = "Articles, Technical Notes, and Translations";
    lang = "ko-KR";
    base_url = "https://example.com";
    github_url = None;
    mode = Blog_mode;
    area_tone = Primary_tone;
    tabs =
      [
        {
          id = "articles";
          label = "생각";
          folder = "articles";
          disclaimer = None;
        };
        { id = "tech"; label = "기술"; folder = "tech"; disclaimer = None };
        {
          id = "translations";
          label = "한국어 번역";
          folder = "translations";
          disclaimer =
            Some
              "번역본에 대한 모든 권리는 원문 저자에게 있습니다. 저자의 요청이 있을 경우 해당 게시글은 언제든지 삭제될 수 있습니다.";
        };
      ];
    nav = [];
    sidebar = [];
    auto_sidebar = false;
    enable_search = true;
    custom_js = None;
    custom_css = None;

    inline_css = true;
  }

let default_docs_config =
  {
    title = "Subux Docs";
    description = "Technical Documentation and Guides";
    lang = "ko-KR";
    base_url = "https://docs.example.com";
    github_url = None;
    mode = Docs_mode;
    area_tone = Primary_tone;
    tabs = [];
    nav =
      [
        { text = "Home"; link = "/" };
        { text = "가이드"; link = "/contents/getting-started" };
      ];
    sidebar =
      [
        {
          text = "시작하기";
          items =
            [
              { text = "기본 가이드"; link = "/contents/getting-started" };
              { text = "설정 안내"; link = "/contents/configuration" };
            ];
        };
      ];
    auto_sidebar = true;
    enable_search = true;
    custom_js = None;
    custom_css = None;

    inline_css = true;
  }
