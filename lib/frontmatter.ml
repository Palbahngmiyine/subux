open Domain
open Domain.Result_syntax

type parsed_markdown = {
  meta : Domain.frontmatter_meta;
  body : string;
}

let strip_utf8_bom s =
  if
    String.length s >= 3
    && Char.code s.[0] = 0xEF
    && Char.code s.[1] = 0xBB
    && Char.code s.[2] = 0xBF
  then String.sub s 3 (String.length s - 3)
  else s

let find_newline s start =
  let len = String.length s in
  let rec loop i =
    if i >= len then None
    else if s.[i] = '\n' then
      let line_end = if i > start && s.[i - 1] = '\r' then i - 1 else i in
      Some (line_end, i + 1)
    else loop (i + 1)
  in
  loop start

let is_spaces_only s start stop =
  let rec loop i =
    if i >= stop then true
    else if s.[i] = ' ' || s.[i] = '\t' then loop (i + 1)
    else false
  in
  loop start

let find_closing_fence s start =
  let len = String.length s in
  let rec loop line_start =
    if line_start > len then None
    else
      match find_newline s line_start with
      | Some (line_end, next_line) ->
          let line_len = line_end - line_start in
          if
            line_len >= 3
            && String.sub s line_start 3 = "---"
            && is_spaces_only s (line_start + 3) line_end
          then Some (line_start, next_line)
          else loop next_line
      | None ->
          let line_len = len - line_start in
          if
            line_len >= 3
            && String.sub s line_start 3 = "---"
            && is_spaces_only s (line_start + 3) len
          then Some (line_start, len)
          else None
  in
  loop start

let split_lines s =
  let len = String.length s in
  let rec loop start acc =
    if start >= len then List.rev acc
    else
      match find_newline s start with
      | Some (line_end, next_start) ->
          let line = String.sub s start (line_end - start) in
          loop next_start (line :: acc)
      | None ->
          let line = String.sub s start (len - start) in
          List.rev (line :: acc)
  in
  loop 0 []

let count_indent line =
  let len = String.length line in
  let rec loop i =
    if i < len && line.[i] = ' ' then loop (i + 1) else i
  in
  loop 0

let strip_inline_comment raw =
  let len = String.length raw in
  let rec quote_start i =
    if i < 0 then true
    else match raw.[i] with
      | ' ' | '\t' -> quote_start (i - 1)
      | '[' | ',' -> true
      | _ -> false
  in
  let rec loop i in_single in_double =
    if i >= len then raw
    else
      let c = raw.[i] in
      if c = '\\' && in_double && i + 1 < len then
        loop (i + 2) in_single in_double
      else if c = '\'' && not in_double && (in_single || quote_start (i - 1)) then
        loop (i + 1) (not in_single) in_double
      else if c = '"' && not in_single && (in_double || quote_start (i - 1)) then
        loop (i + 1) in_single (not in_double)
      else if
        (not in_single) && (not in_double) && c = '#'
        && (i = 0 || raw.[i - 1] = ' ' || raw.[i - 1] = '\t')
      then String.sub raw 0 i
      else loop (i + 1) in_single in_double
  in
  String.trim (loop 0 false false)

let has_unbalanced_brackets_or_quotes s =
  let len = String.length s in
  let structured = len > 0 && (s.[0] = '\'' || s.[0] = '"' || s.[0] = '[' || s.[0] = '{') in
  let rec loop i in_s in_d sq_depth cur_depth =
    if i >= len then in_s || in_d || sq_depth <> 0 || cur_depth <> 0
    else
      let c = s.[i] in
      if c = '\\' && in_d && i + 1 < len then
        loop (i + 2) in_s in_d sq_depth cur_depth
      else if c = '\'' && not in_d then
        loop (i + 1) (not in_s) in_d sq_depth cur_depth
      else if c = '"' && not in_s then
        loop (i + 1) in_s (not in_d) sq_depth cur_depth
      else if not (in_s || in_d) then
        match c with
        | '[' -> loop (i + 1) in_s in_d (sq_depth + 1) cur_depth
        | ']' ->
            if sq_depth <= 0 then true
            else loop (i + 1) in_s in_d (sq_depth - 1) cur_depth
        | '{' -> loop (i + 1) in_s in_d sq_depth (cur_depth + 1)
        | '}' ->
            if cur_depth <= 0 then true
            else loop (i + 1) in_s in_d sq_depth (cur_depth - 1)
        | _ -> loop (i + 1) in_s in_d sq_depth cur_depth
      else loop (i + 1) in_s in_d sq_depth cur_depth
  in
  structured && loop 0 false false 0 0

let has_unsafe_yaml_token v =
  let s = String.trim v in
  if s = "" then false
  else if s.[0] = '"' || s.[0] = '\'' then false
  else
    String.starts_with ~prefix:"!" s
    || String.starts_with ~prefix:"&" s
    || String.starts_with ~prefix:"*" s
    ||
    let len = String.length s in
    let rec scan i =
      if i + 1 >= len then false
      else if
        (s.[i] = ' ' || s.[i] = '[' || s.[i] = '{' || s.[i] = ',')
        && (s.[i + 1] = '&' || s.[i + 1] = '*' || s.[i + 1] = '!')
      then true
      else scan (i + 1)
    in
    scan 0

let unquote_scalar raw =
  let s = String.trim raw in
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then
    let inner = String.sub s 1 (len - 2) in
    let ilen = String.length inner in
    let buf = Buffer.create ilen in
    let rec loop i =
      if i >= ilen then ()
      else if inner.[i] = '\\' && i + 1 < ilen then (
        Buffer.add_char buf inner.[i + 1];
        loop (i + 2))
      else (
        Buffer.add_char buf inner.[i];
        loop (i + 1))
    in
    loop 0;
    Buffer.contents buf
  else if len >= 2 && s.[0] = '\'' && s.[len - 1] = '\'' then
    String.sub s 1 (len - 2)
  else s

let is_key_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' | '@' | '-' -> true
  | _ -> false

let split_key_value trimmed =
  let len = String.length trimmed in
  let rec scan i =
    if i >= len then None
    else if trimmed.[i] = ':' then
      if i = 0 then None
      else if i + 1 = len || trimmed.[i + 1] = ' ' || trimmed.[i + 1] = '\t'
      then
        let key = String.sub trimmed 0 i in
        let rest = String.sub trimmed (i + 1) (len - i - 1) in
        Some (key, strip_inline_comment rest)
      else None
    else if is_key_char trimmed.[i] then scan (i + 1)
    else None
  in
  scan 0

let parse_flow_list s =
  let trimmed = String.trim s in
  let len = String.length trimmed in
  if len >= 2 && trimmed.[0] = '[' && trimmed.[len - 1] = ']' then
    let inner = String.trim (String.sub trimmed 1 (len - 2)) in
    if inner = "" then Some []
    else
      let items =
        List.map
          (fun item -> unquote_scalar (String.trim item))
          (String.split_on_char ',' inner)
      in
      Some items
  else None

let parse_layout = function
  | "home" -> Some Layout_home
  | "doc" -> Some Layout_doc
  | "article" | "post" -> Some Layout_article
  | "page" -> Some Layout_page
  | _ -> None

let apply_top_scalar (meta : frontmatter_meta) key raw_val =
  let v = unquote_scalar raw_val in
  match key with
  | "title" -> { meta with title = Some v }
  | "description" -> { meta with description = Some v }
  | "date" -> { meta with date = parse_date v }
  | "series" -> { meta with series = Some v }
  | "seriesOrder" | "series_order" ->
      { meta with series_order = int_of_string_opt v }
  | "layout" -> { meta with layout = parse_layout v }
  | "order" -> { meta with order = int_of_string_opt v }
  | "draft" ->
      { meta with draft = String.lowercase_ascii v = "true" }
  | "prev" -> { meta with prev_link = Some (v, v) }
  | "next" -> { meta with next_link = Some (v, v) }
  | "tags" -> (
      match parse_flow_list raw_val with
      | Some tags -> { meta with tags }
      | None -> if v = "" then meta else { meta with tags = [ v ] })
  | other ->
      {
        meta with
        extra_fields = (other, v) :: meta.extra_fields;
      }

let rec collect_indented_block base_indent acc = function
  | [] -> (List.rev acc, [])
  | line :: rest as all ->
      let trimmed = String.trim line in
      if trimmed = "" || String.starts_with ~prefix:"#" trimmed then
        collect_indented_block base_indent acc rest
      else
        let indent = count_indent line in
        if indent > base_indent then
          collect_indented_block base_indent ((indent, trimmed) :: acc) rest
        else (List.rev acc, all)

let parse_string_list_block block_lines =
  List.filter_map
    (fun (_indent, trimmed) ->
      if String.starts_with ~prefix:"-" trimmed then
        let item =
          String.trim (String.sub trimmed 1 (String.length trimmed - 1))
        in
        Some (unquote_scalar (strip_inline_comment item))
      else None)
    block_lines

let group_sequence_maps block_lines =
  let rec loop current_map acc = function
    | [] ->
        let final_acc =
          if current_map = [] then acc else List.rev current_map :: acc
        in
        List.rev final_acc
    | (_indent, trimmed) :: rest ->
        if String.starts_with ~prefix:"-" trimmed then
          let after_dash =
            String.trim (String.sub trimmed 1 (String.length trimmed - 1))
          in
          let next_acc =
            if current_map = [] then acc else List.rev current_map :: acc
          in
          match split_key_value after_dash with
          | Some (k, v) -> loop [ (k, unquote_scalar v) ] next_acc rest
          | None -> loop [] next_acc rest
        else (
          match split_key_value trimmed with
          | Some (k, v) ->
              loop ((k, unquote_scalar v) :: current_map) acc rest
          | None -> loop current_map acc rest)
  in
  loop [] [] block_lines

let parse_features_block block_lines =
  let maps = group_sequence_maps block_lines in
  List.filter_map
    (fun fields ->
      match (List.assoc_opt "title" fields, List.assoc_opt "details" fields) with
      | Some title, Some details -> Some { title; details }
      | Some title, None -> Some { title; details = "" }
      | None, _ -> None)
    maps

let parse_hero_block block_lines =
  let rec loop in_actions name text tagline actions_lines = function
    | [] ->
        let actions =
          List.filter_map
            (fun fields ->
              match (List.assoc_opt "text" fields, List.assoc_opt "link" fields) with
              | Some act_text, Some link ->
                  let theme =
                    match List.assoc_opt "theme" fields with
                    | Some "alt" -> Alt_action
                    | _ -> Brand_action
                  in
                  Some { theme; text = act_text; link }
              | _ -> None)
            (group_sequence_maps (List.rev actions_lines))
        in
        { name; text; tagline; actions }
    | (indent, trimmed) :: rest -> (
        if String.starts_with ~prefix:"-" trimmed then
          loop true name text tagline ((indent, trimmed) :: actions_lines) rest
        else
          match split_key_value trimmed with
          | Some ("name", v) ->
              loop false (unquote_scalar v) text tagline actions_lines rest
          | Some ("text", v) when not in_actions ->
              loop false name (Some (unquote_scalar v)) tagline actions_lines rest
          | Some ("tagline", v) ->
              loop false name text (Some (unquote_scalar v)) actions_lines rest
          | Some ("actions", _) ->
              loop true name text tagline actions_lines rest
          | Some ((("theme" | "link" | "text") as k), v) when in_actions ->
              loop true name text tagline
                ((indent, k ^ ": " ^ v) :: actions_lines)
                rest
          | _ -> loop in_actions name text tagline actions_lines rest)
  in
  loop false "" None None [] block_lines

let parse_yaml_lines file lines =
  let rec loop meta = function
    | [] ->
        Ok { meta with extra_fields = List.rev meta.extra_fields }
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "" || String.starts_with ~prefix:"#" trimmed then
          loop meta rest
        else
          let indent = count_indent line in
          if indent <> 0 then
            Error
              (Invalid_frontmatter
                 { file; reason = "Unexpected indentation: " ^ trimmed })
          else if String.starts_with ~prefix:"-" trimmed then
            Error
              (Invalid_frontmatter
                 { file; reason = "YAML frontmatter must be a mapping" })
          else (
            match split_key_value trimmed with
            | None ->
                Error
                  (Invalid_frontmatter
                     { file; reason = "YAML frontmatter must be a mapping" })
            | Some (key, raw_val) ->
                if has_unsafe_yaml_token raw_val then
                  Error
                    (Unsafe_yaml_construct
                       {
                         file;
                         detail =
                           "Tags, anchors, and aliases are rejected in frontmatter";
                       })
                else if has_unbalanced_brackets_or_quotes raw_val then
                  Error
                    (Invalid_frontmatter
                       {
                         file;
                         reason = "Unclosed bracket or quote in value of " ^ key;
                       })
                else if raw_val = "" then
                  let block_lines, remaining =
                    collect_indented_block 0 [] rest
                  in
                  let* () =
                    let has_unsafe =
                      List.exists
                        (fun (_ind, t) ->
                          has_unsafe_yaml_token t
                          || has_unbalanced_brackets_or_quotes t)
                        block_lines
                    in
                    if has_unsafe then
                      Error
                        (Unsafe_yaml_construct
                           {
                             file;
                             detail = "Unsafe or malformed nested YAML in " ^ key;
                           })
                    else Ok ()
                  in
                  let next_meta =
                    match key with
                    | "tags" ->
                        {
                          meta with
                          tags = parse_string_list_block block_lines;
                        }
                    | "features" ->
                        {
                          meta with
                          features = parse_features_block block_lines;
                        }
                    | "hero" ->
                        {
                          meta with
                          hero = Some (parse_hero_block block_lines);
                        }
                    | _ -> meta
                  in
                  loop next_meta remaining
                else loop (apply_top_scalar meta key raw_val) rest)
  in
  loop empty_frontmatter lines

let parse ?(file = "<markdown>") raw =
  let input = strip_utf8_bom raw in
  if not (String.starts_with ~prefix:"---" input) then
    Ok { meta = empty_frontmatter; body = input }
  else
    match find_newline input 0 with
    | None ->
        Error
          (Invalid_frontmatter { file; reason = "Unclosed YAML frontmatter" })
    | Some (first_line_end, after_opening) ->
        let lang =
          String.lowercase_ascii
            (String.trim (String.sub input 3 (first_line_end - 3)))
        in
        if lang <> "" && lang <> "yaml" && lang <> "yml" then
          Error
            (Unsafe_yaml_construct
               { file; detail = "Only YAML frontmatter is supported" })
        else (
          match find_closing_fence input after_opening with
          | None ->
              Error
                (Invalid_frontmatter
                   { file; reason = "Unclosed YAML frontmatter" })
          | Some (closing_start, body_start) ->
              let fm_str =
                String.sub input after_opening (closing_start - after_opening)
              in
              let body =
                String.sub input body_start (String.length input - body_start)
              in
              let lines = split_lines fm_str in
              let+ meta = parse_yaml_lines file lines in
              { meta; body })
