(** Imperative Shell CLI for the Subux Pure Functional Static Site Generator.
    All I/O (filesystem traversal, reading, writing) is isolated in this file. *)

open Subux_lib
open Domain

let read_file_result (path : string) : (string, build_error) result =
  try
    In_channel.with_open_bin path In_channel.input_all |> Result.ok
  with Sys_error msg -> Error (Io_error msg)

let rec mkdir_p (dir : string) : unit =
  if dir = "" || dir = "." || dir = "/" then ()
  else if Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file_result (path : string) (content : string) :
    (unit, build_error) result =
  try
    mkdir_p (Filename.dirname path);
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc content);
    Ok ()
  with
  | Sys_error msg -> Error (Io_error msg)
  | Unix.Unix_error (err, fn, arg) ->
      Error (Io_error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err)))

let is_markdown_file (name : string) : bool =
  let lower = String.lowercase_ascii name in
  String.ends_with ~suffix:".md" lower || String.ends_with ~suffix:".mdx" lower

let rec collect_markdown_files_in_dir ~(base_dir : string) ~(collection : string)
    (rel_prefix : string) : Site_builder.source_file list =
  let full_dir =
    if rel_prefix = "" then base_dir else Filename.concat base_dir rel_prefix
  in
  if not (Sys.file_exists full_dir && Sys.is_directory full_dir) then []
  else
    let entries =
      Sys.readdir full_dir |> Array.to_list |> List.sort String.compare
    in
    List.concat_map
      (fun name ->
        if String.starts_with ~prefix:"." name || name = "_site" || name = "dist"
           || name = "_build" || name = "vendor" || name = "node_modules"
        then []
        else
          let rel =
            if rel_prefix = "" then name else rel_prefix ^ "/" ^ name
          in
          let full = Filename.concat full_dir name in
          if Sys.is_directory full then
            collect_markdown_files_in_dir ~base_dir ~collection rel
          else if is_markdown_file name then
            match read_file_result full with
            | Ok raw_markdown ->
                let rel_path =
                  if collection = "" then rel
                  else collection ^ "/" ^ rel
                in
                [ { Site_builder.collection; rel_path; raw_markdown } ]
            | Error _ -> []
          else [])
      entries

let discover_sources (input_dir : string) (config : site_config) :
    Site_builder.source_file list =
  match config.mode with
  | Blog_mode ->
      let tabs =
        if config.tabs = [] then default_blog_config.tabs else config.tabs
      in
      let tab_sources =
        List.concat_map
          (fun (t : collection_tab) ->
            let col_dir = Filename.concat input_dir t.folder in
            collect_markdown_files_in_dir ~base_dir:col_dir ~collection:t.folder
              "")
          tabs
      in
      if tab_sources <> [] then tab_sources
      else
        collect_markdown_files_in_dir ~base_dir:input_dir ~collection:"articles"
          ""
  | Docs_mode ->
      let root_index =
        let idx_path = Filename.concat input_dir "index.md" in
        if Sys.file_exists idx_path then
          match read_file_result idx_path with
          | Ok raw_markdown ->
              [ { Site_builder.collection = ""; rel_path = "index.md"; raw_markdown } ]
          | Error _ -> []
        else []
      in
      let contents_dir = Filename.concat input_dir "contents" in
      let docs_dir = Filename.concat input_dir "docs" in
      let sub_sources =
        if Sys.file_exists contents_dir && Sys.is_directory contents_dir then
          collect_markdown_files_in_dir ~base_dir:contents_dir ~collection:"" ""
          |> List.map (fun (s : Site_builder.source_file) ->
                 { s with rel_path = "contents/" ^ s.rel_path })
        else if Sys.file_exists docs_dir && Sys.is_directory docs_dir then
          collect_markdown_files_in_dir ~base_dir:docs_dir ~collection:"" ""
          |> List.map (fun (s : Site_builder.source_file) ->
                 { s with rel_path = "docs/" ^ s.rel_path })
        else
          collect_markdown_files_in_dir ~base_dir:input_dir ~collection:"" ""
          |> List.filter (fun (s : Site_builder.source_file) ->
                 s.rel_path <> "index.md"
                 && s.rel_path <> "CLAUDE.md"
                 && s.rel_path <> "README.md")
      in
      root_index @ sub_sources
  | Unified_mode ->
      let tabs =
        if config.tabs = [] then default_blog_config.tabs else config.tabs
      in
      let tab_sources =
        List.concat_map
          (fun (t : collection_tab) ->
            let col_dir = Filename.concat input_dir t.folder in
            collect_markdown_files_in_dir ~base_dir:col_dir ~collection:t.folder
              "")
          tabs
      in
      let contents_dir = Filename.concat input_dir "contents" in
      let doc_sources =
        if Sys.file_exists contents_dir && Sys.is_directory contents_dir then
          collect_markdown_files_in_dir ~base_dir:contents_dir
            ~collection:"contents" ""
        else []
      in
      tab_sources @ doc_sources

let parse_config_json (base : site_config) (raw_json : string) :
    (site_config, build_error) result =
  let open Result_syntax in
  let* j = Json_parser.parse raw_json in
  match j with
  | Json_parser.Object fields ->
      let get_str k default =
        match List.assoc_opt k fields with
        | Some (Json_parser.String s) -> s
        | _ -> default
      in
      let mode =
        match get_str "mode" "" with
        | "blog" -> Blog_mode
        | "docs" -> Docs_mode
        | "unified" -> Unified_mode
        | _ -> base.mode
      in
      let base =
        match mode with
        | Docs_mode when base.mode = Blog_mode ->
            { default_docs_config with mode = Docs_mode }
        | _ -> { base with mode }
      in
      let area_tone =
        match get_str "areaTone" "" with
        | "accent" -> Accent_tone
        | "primary" -> Primary_tone
        | _ -> base.area_tone
      in
      let github_url =
        match List.assoc_opt "githubUrl" fields with
        | Some (Json_parser.String s) when s <> "" -> Some s
        | Some Json_parser.Null -> None
        | _ -> base.github_url
      in
      let nav =
        match List.assoc_opt "nav" fields with
        | Some (Json_parser.Array items) ->
            List.filter_map
              (function
                | Json_parser.Object nf ->
                    begin match
                      (List.assoc_opt "text" nf, List.assoc_opt "link" nf)
                    with
                    | Some (Json_parser.String t), Some (Json_parser.String l) ->
                        Some ({ text = t; link = l } : nav_item)
                    | _ -> None
                    end
                | _ -> None)
              items
        | _ -> base.nav
      in
      let sidebar =
        match List.assoc_opt "sidebar" fields with
        | Some (Json_parser.Array groups) ->
            List.filter_map
              (function
                | Json_parser.Object gf ->
                    let g_text =
                      match List.assoc_opt "text" gf with
                      | Some (Json_parser.String s) -> s
                      | _ -> "Section"
                    in
                    let g_items =
                      match List.assoc_opt "items" gf with
                      | Some (Json_parser.Array entries) ->
                          List.filter_map
                            (function
                              | Json_parser.Object ef ->
                                  begin match
                                    ( List.assoc_opt "text" ef,
                                      List.assoc_opt "link" ef )
                                  with
                                  | ( Some (Json_parser.String t),
                                      Some (Json_parser.String l) ) ->
                                      Some ({ text = t; link = l } : sidebar_entry)
                                  | _ -> None
                                  end
                              | _ -> None)
                            entries
                      | _ -> []
                    in
                    Some ({ text = g_text; items = g_items } : sidebar_group)
                | _ -> None)
              groups
        | _ -> base.sidebar
      in
      Ok
        {
          base with
          title = get_str "title" base.title;
          description = get_str "description" base.description;
          lang = get_str "lang" base.lang;
          base_url = get_str "baseUrl" base.base_url;
          github_url;
          enable_search = (match List.assoc_opt "enableSearch" fields with
            | Some (Json_parser.Bool value) -> value | _ -> base.enable_search);
          mode;
          area_tone;
          nav;
          sidebar;
        }
  | _ -> Error (Config_error "subux.json root must be a JSON object")

let resolve_tokens_path (explicit : string option) (input_dir : string) : string =
  match explicit with
  | Some p -> p
  | None ->
      let candidate1 = Filename.concat input_dir "design/tokens.json" in
      let candidate2 = Filename.concat input_dir "tokens.json" in
      let candidate3 = "design/tokens.json" in
      if Sys.file_exists candidate1 then candidate1
      else if Sys.file_exists candidate2 then candidate2
      else candidate3

type cli_options = {
  subcommand : string;
  input_dir : string;
  output_dir : string;
  tokens_path : string option;
  mode_override : site_mode option;
  tone_override : area_tone option;
  title_override : string option;
}

let parse_cli_args (argv : string list) : cli_options =
  let default_opts =
    {
      subcommand = "build";
      input_dir = ".";
      output_dir = "_site";
      tokens_path = None;
      mode_override = None;
      tone_override = None;
      title_override = None;
    }
  in
  let rec loop opts = function
    | [] -> opts
    | ("build" | "tokens" | "check" | "init" | "help" | "--help" | "-h") as cmd
      :: rest ->
        loop { opts with subcommand = cmd } rest
    | ("--input" | "-i") :: dir :: rest ->
        loop { opts with input_dir = dir } rest
    | ("--output" | "-o") :: dir :: rest ->
        loop { opts with output_dir = dir } rest
    | "--tokens" :: path :: rest ->
        loop { opts with tokens_path = Some path } rest
    | "--mode" :: m :: rest ->
        let mode_override =
          match String.lowercase_ascii m with
          | "blog" -> Some Blog_mode
          | "docs" -> Some Docs_mode
          | "unified" -> Some Unified_mode
          | _ -> opts.mode_override
        in
        loop { opts with mode_override } rest
    | "--tone" :: t :: rest ->
        let tone_override =
          match String.lowercase_ascii t with
          | "accent" -> Some Accent_tone
          | "primary" -> Some Primary_tone
          | _ -> opts.tone_override
        in
        loop { opts with tone_override } rest
    | "--title" :: title :: rest ->
        loop { opts with title_override = Some title } rest
    | arg :: rest ->
        if not (String.starts_with ~prefix:"-" arg) && opts.input_dir = "." then
          loop { opts with input_dir = arg } rest
        else loop opts rest
  in
  loop default_opts argv

let build_effective_config (opts : cli_options) :
    (site_config, build_error) result =
  let open Result_syntax in
  let initial_base =
    match opts.mode_override with
    | Some Docs_mode -> default_docs_config
    | Some Unified_mode -> { default_blog_config with mode = Unified_mode }
    | Some Blog_mode | None -> default_blog_config
  in
  let config_file = Filename.concat opts.input_dir "subux.json" in
  let* from_file =
    if Sys.file_exists config_file then
      let* raw = read_file_result config_file in
      let* config = parse_config_json initial_base raw in
      let* json = Json_parser.parse raw in
      (match json with
       | Json_parser.Object fields ->
           let load_asset key =
             match List.assoc_opt key fields with
             | None | Some Json_parser.Null -> Ok None
             | Some (Json_parser.String path) ->
                 if path = "" || not (Filename.is_relative path)
                    || List.exists (fun part -> part = "..") (String.split_on_char '/' path)
                    || String.contains path '\\'
                 then Error (Config_error (key ^ " must be a relative path inside the input directory"))
                 else
                   let* resolved =
                     try
                       let base = Unix.realpath opts.input_dir in
                       let file = Unix.realpath (Filename.concat base path) in
                       let prefix = if base = "/" then base else base ^ "/" in
                       if String.starts_with ~prefix file && (Unix.stat file).Unix.st_kind = Unix.S_REG
                       then Ok file
                       else Error (Config_error (key ^ " must resolve to a regular file inside the input directory"))
                     with
                     | Sys_error msg -> Error (Io_error msg)
                     | Unix.Unix_error (err, fn, arg) ->
                         Error (Io_error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err)))
                   in
                   let* content = read_file_result resolved in
                   Ok (Some content)
             | Some _ -> Error (Config_error (key ^ " must be a relative file path"))
           in
           let* custom_css = load_asset "customCss" in
           let* custom_js = load_asset "customJs" in
           Ok { config with custom_css; custom_js }
       | _ -> Ok config)
    else Ok initial_base
  in
  let with_mode =
    match opts.mode_override with
    | Some m -> { from_file with mode = m }
    | None -> from_file
  in
  let with_tone =
    match opts.tone_override with
    | Some t -> { with_mode with area_tone = t }
    | None -> with_mode
  in
  let with_title =
    match opts.title_override with
    | Some title -> { with_tone with title }
    | None -> with_tone
  in
  Ok with_title

let run_init (opts : cli_options) : (unit, build_error) result =
  let open Result_syntax in
  let dir = if opts.input_dir = "." then "site" else opts.input_dir in
  let mode = Option.value ~default:Blog_mode opts.mode_override in
  match mode with
  | Blog_mode | Unified_mode ->
      let config_json =
        "{\n\
        \  \"title\": \"Subux Blog\",\n\
        \  \"description\": \"Articles and Technical Notes\",\n\
        \  \"lang\": \"ko\",\n\
        \  \"baseUrl\": \"https://example.com\",\n\
        \  \"githubUrl\": null,\n\
        \  \"mode\": \"blog\",\n\
        \  \"areaTone\": \"primary\"\n\
         }\n"
      in
      let sample_article =
        "---\n\
         title: 시작하기\n\
         date: 2026-02-15\n\
         series: 기본 가이드\n\
         seriesOrder: 1\n\
         ---\n\n\
         > [!note]+ 요약\n\
         > 순수 함수형 정적 사이트 생성기 **Subux**로 렌더링된 첫 번째 글입니다.[^1]\n\n\
         [[2026/architecture-overview|아키텍처 개요]] 문서와 연결됩니다.\n\n\
         [^1]: 각주 호버 툴팁과 시맨틱 디자인 토큰이 자동 적용됩니다.\n"
      in
      let* () =
        write_file_result (Filename.concat dir "subux.json") config_json
      in
      let* () =
        write_file_result
          (Filename.concat dir "articles/2026/getting-started.md")
          sample_article
      in
      Printf.printf "Initialized Subux blog workspace in %s\n" dir;
      Ok ()
  | Docs_mode ->
      let config_json =
        "{\n\
        \  \"title\": \"Subux Docs\",\n\
        \  \"description\": \"Technical Documentation and Guides\",\n\
        \  \"lang\": \"ko-KR\",\n\
        \  \"baseUrl\": \"https://docs.example.com\",\n\
        \  \"githubUrl\": null,\n\
        \  \"mode\": \"docs\",\n\
        \  \"areaTone\": \"primary\"\n\
         }\n"
      in
      let sample_home =
        "---\n\
         layout: home\n\
         hero:\n\
        \  name: \"Subux Docs\"\n\
        \  text: \"기술 문서 가이드\"\n\
        \  tagline: \"체계적인 기술 문서와 레퍼런스를 작성합니다\"\n\
        \  actions:\n\
        \    - theme: brand\n\
        \      text: 시작하기\n\
        \      link: /contents/getting-started\n\
         features:\n\
        \  - title: 순수 함수형 파이프라인\n\
        \    details: 예측 가능한 정적 빌드와 타입 안전성을 제공합니다.\n\
        \  - title: 시맨틱 디자인 토큰\n\
        \    details: 라이트·다크 모드와 접근성 명암비를 자동으로 보장합니다.\n\
         ---\n"
      in
      let sample_doc =
        "---\n\
         title: 시작하기\n\
         order: 1\n\
         ---\n\n\
         # 시작하기\n\n\
         ## 핵심 원칙\n\n\
         - 간결하고 검증 가능한 구조로 문서를 작성합니다.\n"
      in
      let* () =
        write_file_result (Filename.concat dir "subux.json") config_json
      in
      let* () =
        write_file_result (Filename.concat dir "index.md") sample_home
      in
      let* () =
        write_file_result
          (Filename.concat dir "contents/getting-started.md")
          sample_doc
      in
      Printf.printf "Initialized Subux docs workspace in %s\n" dir;
      Ok ()

let print_usage () : unit =
  print_endline
    "Subux — Pure Functional Static Site Generator\n\n\
     Usage:\n\
    \  subux build  [--mode blog|docs|unified] [--tone primary|accent] [-i <dir>] [-o <dir>] [--tokens <file>]\n\
    \  subux tokens [--tokens <file>] [--tone primary|accent] [-o <css-file>]\n\
    \  subux check  [-i <dir>] [--tokens <file>]\n\
    \  subux init   [--mode blog|docs] <dir>\n"

let run () : (unit, build_error) result =
  let open Result_syntax in
  let argv = Array.to_list Sys.argv |> List.tl in
  let opts = parse_cli_args argv in
  match opts.subcommand with
  | "help" | "--help" | "-h" ->
      print_usage ();
      Ok ()
  | "init" -> run_init opts
  | "tokens" ->
      let t_path = resolve_tokens_path opts.tokens_path opts.input_dir in
      let* raw_tokens = read_file_result t_path in
      let area_tone = Option.value ~default:Primary_tone opts.tone_override in
      let* css = Token_compiler.compile_full_stylesheet ~area_tone raw_tokens in
      if opts.output_dir <> "_site" then
        write_file_result opts.output_dir css
      else begin
        print_string css;
        Ok ()
      end
  | "check" ->
      let* config = build_effective_config opts in
      let t_path = resolve_tokens_path opts.tokens_path opts.input_dir in
      let* tokens_json = read_file_result t_path in
      let sources = discover_sources opts.input_dir config in
      let* plan = Site_builder.compile_site ~config ~tokens_json sources in
      Printf.printf "OK: Checked %d markdown entries and %d generated artifacts.\n"
        (List.length plan.entries)
        (List.length plan.artifacts);
      Ok ()
  | _ ->
      let* config = build_effective_config opts in
      let t_path = resolve_tokens_path opts.tokens_path opts.input_dir in
      let* tokens_json = read_file_result t_path in
      let sources = discover_sources opts.input_dir config in
      let* plan = Site_builder.compile_site ~config ~tokens_json sources in
      let* _ =
        map_list
          (fun (art : Site_builder.output_artifact) ->
            let full_out = Filename.concat opts.output_dir art.out_path in
            write_file_result full_out art.content)
          plan.artifacts
      in
      Printf.printf
        "Built %d entries -> %d static artifacts in %s\n"
        (List.length plan.entries)
        (List.length plan.artifacts)
        opts.output_dir;
      Ok ()

let () =
  match run () with
  | Ok () -> ()
  | Error err ->
      prerr_endline ("Error: " ^ error_to_string err);
      exit 1
