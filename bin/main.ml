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

(* True when [dir] is the public directory, whose files are copied unrendered. *)
let is_excluded (exclude : string option) (dir : string) : bool =
  match exclude with
  | None -> false
  | Some root -> (try Unix.realpath dir = root with Unix.Unix_error _ -> false)

(* Walk Markdown sources, following directory links. [ancestors] holds the
   (device, inode) of each directory on the current path, so a link back to an
   ancestor ends the walk instead of looping. Entries that cannot be resolved,
   such as dangling links, and anything but regular files are skipped, which
   matches how unreadable files are already skipped. *)
let rec collect_markdown_files_in_dir ?exclude ?(ancestors = []) ~(base_dir : string)
    ~(collection : string) (rel_prefix : string) : Site_builder.source_file list =
  let full_dir =
    if rel_prefix = "" then base_dir else Filename.concat base_dir rel_prefix
  in
  match Unix.stat full_dir with
  | exception Unix.Unix_error _ -> []
  | { Unix.st_kind = Unix.S_DIR; st_dev; st_ino; _ } ->
      let id = (st_dev, st_ino) in
      if List.mem id ancestors || is_excluded exclude full_dir then []
      else
        let entries =
          try Sys.readdir full_dir |> Array.to_list |> List.sort String.compare
          with Sys_error _ -> []
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
              match Unix.stat full with
              | exception Unix.Unix_error _ -> []
              | { Unix.st_kind = Unix.S_DIR; _ } ->
                  collect_markdown_files_in_dir ?exclude ~ancestors:(id :: ancestors)
                    ~base_dir ~collection rel
              | { Unix.st_kind = Unix.S_REG; _ } when is_markdown_file name ->
                  begin match read_file_result full with
                  | Ok raw_markdown ->
                      let rel_path =
                        if collection = "" then rel else collection ^ "/" ^ rel
                      in
                      [ { Site_builder.collection; rel_path; raw_markdown } ]
                  | Error _ -> []
                  end
              | _ -> [])
          entries
  | _ -> []

let discover_sources ?exclude (input_dir : string) (config : site_config) :
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
            collect_markdown_files_in_dir ?exclude ~base_dir:col_dir ~collection:t.folder
              "")
          tabs
      in
      if tab_sources <> [] then tab_sources
      else
        collect_markdown_files_in_dir ?exclude ~base_dir:input_dir ~collection:"articles"
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
          collect_markdown_files_in_dir ?exclude ~base_dir:contents_dir ~collection:"" ""
          |> List.map (fun (s : Site_builder.source_file) ->
                 { s with rel_path = "contents/" ^ s.rel_path })
        else if Sys.file_exists docs_dir && Sys.is_directory docs_dir then
          collect_markdown_files_in_dir ?exclude ~base_dir:docs_dir ~collection:"" ""
          |> List.map (fun (s : Site_builder.source_file) ->
                 { s with rel_path = "docs/" ^ s.rel_path })
        else
          collect_markdown_files_in_dir ?exclude ~base_dir:input_dir ~collection:"" ""
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
            collect_markdown_files_in_dir ?exclude ~base_dir:col_dir ~collection:t.folder
              "")
          tabs
      in
      let contents_dir = Filename.concat input_dir "contents" in
      let doc_sources =
        if Sys.file_exists contents_dir && Sys.is_directory contents_dir then
          collect_markdown_files_in_dir ?exclude ~base_dir:contents_dir
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
      let* head_links =
        match List.assoc_opt "headLinks" fields with
        | None -> Ok base.head_links
        | Some (Json_parser.Array items) ->
            map_list
              (function
                | Json_parser.Object lf ->
                    let field key =
                      match List.assoc_opt key lf with
                      | None -> Ok None
                      | Some (Json_parser.String v) -> Ok (Some v)
                      | Some _ -> Error (Config_error ("headLinks " ^ key ^ " must be a string"))
                    in
                    let* () =
                      if List.for_all (fun (k, _) -> List.mem k [ "rel"; "href"; "type"; "sizes" ]) lf
                      then Ok ()
                      else Error (Config_error "headLinks entries accept only rel, href, type and sizes")
                    in
                    let* rel = field "rel" in
                    let* href = field "href" in
                    let* link_type = field "type" in
                    let* sizes = field "sizes" in
                    begin match rel, href with
                    | Some rel, Some href -> make_head_link ~rel ~href ~link_type ~sizes
                    | _ -> Error (Config_error "headLinks entries require rel and href")
                    end
                | _ -> Error (Config_error "headLinks entries must be objects"))
              items
        | Some _ -> Error (Config_error "headLinks must be an array")
      in
      let* theme_color =
        match List.assoc_opt "themeColor" fields with
        | None -> Ok base.theme_color
        | Some Json_parser.Null -> Ok None
        | Some (Json_parser.String color) -> Result.map Option.some (make_theme_color color)
        | Some _ -> Error (Config_error "themeColor must be a string")
      in
      let* font_display =
        match List.assoc_opt "fontDisplay" fields with
        | None -> Ok base.font_display
        | Some (Json_parser.String "optional") -> Ok Font_optional
        | Some (Json_parser.String "swap") -> Ok Font_swap
        | Some _ -> Error (Config_error "fontDisplay must be \"optional\" or \"swap\"")
      in
      let flag key default =
        match List.assoc_opt key fields with
        | None -> Ok default
        | Some (Json_parser.Bool value) -> Ok value
        | Some _ -> Error (Config_error (key ^ " must be true or false"))
      in
      let* lazy_images = flag "lazyImages" base.lazy_images in
      let* trailing_slash = flag "trailingSlash" base.trailing_slash in
      Ok
        {
          base with
          title = get_str "title" base.title;
          description = get_str "description" base.description;
          lang = get_str "lang" base.lang;
          base_url = get_str "baseUrl" base.base_url;
          auto_sidebar = (match List.assoc_opt "autoSidebar" fields with
            | Some (Json_parser.Bool b) -> b | _ -> base.auto_sidebar);
          enable_search = (match List.assoc_opt "enableSearch" fields with
            | Some (Json_parser.Bool b) -> b | _ -> base.enable_search);
          inline_css = (match List.assoc_opt "inlineCss" fields with
            | Some (Json_parser.Bool b) -> b | _ -> base.inline_css);
          github_url;
          mode;
          area_tone;
          nav;
          sidebar;
          head_links;
          theme_color;
          font_display;
          lazy_images;
          trailing_slash;
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
  adapter : Adapter.t;
  cloudflare_name : string;
  compatibility_date : string;
}

let parse_cli_args (argv : string list) : (cli_options, build_error) result =
  let default_opts =
    {
      subcommand = "build";
      input_dir = ".";
      output_dir = "_site";
      tokens_path = None;
      mode_override = None;
      tone_override = None;
      title_override = None;
      adapter = Adapter.Static;
      cloudflare_name = "subux-site";
      compatibility_date = "2026-10-29";
    }
  in
  let rec loop opts = function
    | [] -> Ok opts
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
    | "--adapter" :: adapter :: rest ->
        let open Result_syntax in
        let* adapter = Adapter.of_string adapter in
        loop { opts with adapter } rest
    | "--name" :: cloudflare_name :: rest ->
        loop { opts with cloudflare_name } rest
    | "--compatibility-date" :: compatibility_date :: rest ->
        loop { opts with compatibility_date } rest
    | [ ("--adapter" | "--name" | "--compatibility-date") as flag ] ->
        Error (Config_error ("Missing value for " ^ flag))
    | arg :: rest ->
        if not (String.starts_with ~prefix:"-" arg) && opts.input_dir = "." then
          loop { opts with input_dir = arg } rest
        else loop opts rest
  in
  loop default_opts argv

(* Replace only adapter-owned Build Output. A fresh tree prevents removed or
   newly drafted pages from surviving a rebuild. Never follow output symlinks. *)
let write_cloudflare_output output_dir artifacts : (unit, build_error) result =
  let output_dir =
    let rec trim_end n =
      if n > 1 && output_dir.[n - 1] = '/' then trim_end (n - 1) else n
    in
    if output_dir = "" then "."
    else String.sub output_dir 0 (trim_end (String.length output_dir))
  in
  let marker_content = "Subux Cloudflare adapter v0\n" in
  let target = Filename.concat output_dir Adapter.cloudflare_output_dir in
  let marker dir = Filename.concat dir ".subux-generated" in
  let rec remove_tree path =
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Array.iter (fun name -> remove_tree (Filename.concat path name))
          (Sys.readdir path);
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  let rec check_directories path =
    let open Result_syntax in
    let* () =
      if path = "." || path = "/" || path = "" then Ok ()
      else check_directories (Filename.dirname path)
    in
    try
      if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then Ok ()
      else Error (Io_error ("Adapter output must be a real directory: " ^ path))
    with Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  in
  try
    let open Result_syntax in
    let* () = check_directories target in
    let exists = Sys.file_exists target in
    let* () =
      if not exists then Ok ()
      else
        let owned =
          try
            (Unix.lstat (marker target)).Unix.st_kind = Unix.S_REG
            && read_file_result (marker target) = Ok marker_content
          with Unix.Unix_error (Unix.ENOENT, _, _) -> false
        in
        if owned then Ok ()
        else Error (Io_error ("Refusing to replace non-Subux output: " ^ target))
    in
    let parent = Filename.dirname target in
    mkdir_p parent;
    let staging = Filename.temp_file ~temp_dir:parent ".subux-" "" in
    Unix.unlink staging;
    Unix.mkdir staging 0o755;
    let backup = Filename.concat staging "previous" in
    Fun.protect
      ~finally:(fun () ->
        (* Retain the backup if restoring a failed publish also fails. *)
        if not (Sys.file_exists backup) then remove_tree staging)
      (fun () ->
      let fresh = Filename.concat staging "v0" in
      let prefix = Adapter.cloudflare_output_dir ^ "/" in
      let* _ = map_list (fun (a : Site_builder.output_artifact) ->
        let path = String.sub a.out_path (String.length prefix)
            (String.length a.out_path - String.length prefix) in
        write_file_result (Filename.concat fresh path) a.content) artifacts in
      let* () = write_file_result (marker fresh) marker_content in
      if exists then Unix.rename target backup;
      begin try Unix.rename fresh target with exn ->
        if exists then Unix.rename backup target;
        raise exn
      end;
      if exists then remove_tree backup;
      Ok ())
  with
  | Sys_error msg -> Error (Io_error msg)
  | Unix.Unix_error (err, fn, arg) ->
      Error (Io_error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err)))

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

let unix_error (err, fn, arg) =
  Io_error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err))

(* Resolve [publicDir] (default "public"; null disables it) to a real directory
   inside the input directory. A missing default directory is not an error. *)
let resolve_public_dir (input_dir : string) : (string option, build_error) result =
  let open Result_syntax in
  let config_file = Filename.concat input_dir "subux.json" in
  let* setting =
    if not (Sys.file_exists config_file) then Ok (Some ("public", false))
    else
      let* raw = read_file_result config_file in
      let* json = Json_parser.parse raw in
      match json with
      | Json_parser.Object fields ->
          begin match List.assoc_opt "publicDir" fields with
          | None -> Ok (Some ("public", false))
          | Some Json_parser.Null -> Ok None
          | Some (Json_parser.String path) -> Ok (Some (path, true))
          | Some _ -> Error (Config_error "publicDir must be a relative directory path or null")
          end
      | _ -> Error (Config_error "subux.json root must be a JSON object")
  in
  match setting with
  | None -> Ok None
  | Some (path, explicit) ->
      if path = "" || not (Filename.is_relative path)
         || List.exists (fun part -> part = "..") (String.split_on_char '/' path)
         || String.contains path '\\'
      then Error (Config_error "publicDir must be a relative path inside the input directory")
      else
        try
          let base = Unix.realpath input_dir in
          let candidate = Filename.concat base path in
          if not (Sys.file_exists candidate) then
            if explicit then Error (Config_error ("publicDir does not exist: " ^ path))
            else Ok None
          else
            if not explicit && not (Sys.is_directory candidate) then Ok None else
            let dir = Unix.realpath candidate in
            let prefix = if base = "/" then base else base ^ "/" in
            if String.starts_with ~prefix dir && (Unix.stat dir).Unix.st_kind = Unix.S_DIR
            then Ok (Some dir)
            else Error (Config_error "publicDir must resolve to a directory inside the input directory")
        with
        | Sys_error msg -> Error (Io_error msg)
        | Unix.Unix_error (err, fn, arg) -> Error (unix_error (err, fn, arg))

(* Read every regular file under [root] without following links. Links and
   special files are rejected before opening, so a FIFO cannot block a build.
   Entries whose name starts with a dot are skipped without being inspected,
   which keeps editor and OS metadata such as .DS_Store out of the site; the
   .well-known directory is the one exception. *)
let read_public_files (root : string) :
    (Site_builder.output_artifact list, build_error) result =
  let open Result_syntax in
  let rec walk rel =
    let full = if rel = "" then root else Filename.concat root rel in
    match (Unix.lstat full).Unix.st_kind with
    | Unix.S_DIR ->
        let names =
          Sys.readdir full |> Array.to_list
          |> List.filter (fun name ->
                 name = ".well-known" || not (String.starts_with ~prefix:"." name))
          |> List.sort String.compare
        in
        let+ nested = map_list (fun name ->
          walk (if rel = "" then name else rel ^ "/" ^ name)) names in
        List.concat nested
    | Unix.S_REG ->
        let+ content = read_file_result full in
        [ { Site_builder.out_path = rel; content } ]
    | _ ->
        Error (Config_error
          ("publicDir entries must be regular files or directories: " ^ rel))
  in
  try walk "" with
  | Sys_error msg -> Error (Io_error msg)
  | Unix.Unix_error (err, fn, arg) -> Error (unix_error (err, fn, arg))

(* Render the site, then add public files before deployment packaging. *)
let plan_site (opts : cli_options) (config : site_config) :
    (Site_builder.build_plan * Site_builder.output_artifact list, build_error) result =
  let open Result_syntax in
  let t_path = resolve_tokens_path opts.tokens_path opts.input_dir in
  let* tokens_json = read_file_result t_path in
  let* public_root = resolve_public_dir opts.input_dir in
  let sources = discover_sources ?exclude:public_root opts.input_dir config in
  let* plan = Site_builder.compile_site ~config ~tokens_json sources in
  let* public = match public_root with
    | None -> Ok []
    | Some root -> read_public_files root
  in
  let* site = Site_builder.merge_public_files ~generated:plan.artifacts public in
  let+ artifacts = Adapter.adapt opts.adapter ~name:opts.cloudflare_name
      ~compatibility_date:opts.compatibility_date site in
  (plan, artifacts)

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
    \  subux init   [--mode blog|docs] <dir>\n";
  print_endline
    "Cloudflare Workers Static Assets:\n\
    \  subux build --adapter cloudflare [--name subux-site] [--compatibility-date YYYY-MM-DD] [-i <dir>] [-o <dir>]\n\
    \  cd <output-dir> && cf deploy --prebuilt\n"

let run () : (unit, build_error) result =
  let open Result_syntax in
  let argv = Array.to_list Sys.argv |> List.tl in
  let* opts = parse_cli_args argv in
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
      let* plan, artifacts = plan_site opts config in
      Printf.printf "OK: Checked %d markdown entries and %d generated artifacts.\n"
        (List.length plan.entries)
        (List.length artifacts);
      Ok ()
  | _ ->
      let* config = build_effective_config opts in
      let* plan, artifacts = plan_site opts config in
      let* () = match opts.adapter with
        | Adapter.Cloudflare -> write_cloudflare_output opts.output_dir artifacts
        | Adapter.Static ->
          let* _ = map_list
          (fun (art : Site_builder.output_artifact) ->
            let full_out = Filename.concat opts.output_dir art.out_path in
            write_file_result full_out art.content)
          artifacts in
          Ok ()
      in
      Printf.printf
        "Built %d entries -> %d static artifacts in %s\n"
        (List.length plan.entries)
        (List.length artifacts)
        opts.output_dir;
      begin match opts.adapter with
      | Adapter.Static -> ()
      | Adapter.Cloudflare ->
          Printf.printf "Deploy: cd %s && cf deploy --prebuilt\n"
            (Filename.quote opts.output_dir)
      end;
      Ok ()

let () =
  match run () with
  | Ok () -> ()
  | Error err ->
      prerr_endline ("Error: " ^ error_to_string err);
      exit 1
