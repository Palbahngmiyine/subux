let ensure condition message =
  if not condition then failwith message

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let write_file path content =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
    Out_channel.output_string channel content)

let exists path =
  try ignore (Unix.lstat path); true
  with Unix.Unix_error (Unix.ENOENT, _, _) -> false

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let rec files root relative =
  let path = Filename.concat root relative in
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_REG -> [ relative ]
  | Unix.S_DIR ->
      Sys.readdir path |> Array.to_list |> List.sort String.compare
      |> List.concat_map (fun name -> files root (Filename.concat relative name))
  | _ -> failwith ("Unexpected file type in generated output: " ^ path)

let compare_trees expected actual =
  let paths = files expected "" in
  ensure (paths = files actual "") "Static and Cloudflare file lists differ";
  List.iter (fun path ->
    ensure (read_file (Filename.concat expected path)
            = read_file (Filename.concat actual path))
      ("Static and Cloudflare file contents differ: " ^ path)) paths

let rec waitpid pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid pid

let run_command ~executable ~log ~expect_success args =
  let descriptor = Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let status = Fun.protect ~finally:(fun () -> Unix.close descriptor) (fun () ->
    let pid = Unix.create_process executable
        (Array.of_list (executable :: args)) Unix.stdin descriptor descriptor in
    waitpid pid)
  in
  let accepted = match status with
    | Unix.WEXITED code -> if expect_success then code = 0 else code <> 0
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> false
  in
  if not accepted then begin
    let actual = match status with
      | Unix.WEXITED code -> Printf.sprintf "exit %d" code
      | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
      | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal
    in
    failwith (Printf.sprintf "CLI %s: expected %s, got %s\n%s"
      (String.concat " " (List.map Filename.quote args))
      (if expect_success then "success" else "a non-zero exit") actual (read_file log))
  end

let test_workspace ~executable ~tokens_path work =
  let path = Filename.concat work in
  let write relative content = write_file (path relative) content in
  let present relative = ensure (exists (path relative)) ("Missing file: " ^ relative) in
  let absent relative = ensure (not (exists (path relative))) ("Unexpected output: " ^ relative) in
  let invoke = run_command ~executable ~log:(path "command.log") in
  let build ~expect_success args =
    invoke ~expect_success
      ([ "build"; "-i"; path "input"; "--tokens"; path "토큰 파일.json" ] @ args)
  in
  let cloudflare output = [ "--adapter"; "cloudflare"; "-o"; path output ] in
  let assets = "cf-blog/.cloudflare/output/v0/workers/default/assets/" in
  let asset relative = assets ^ relative in
  write "토큰 파일.json" (read_file tokens_path);
  write "input/index.md" "# Home\n";
  write "input/articles/post.md" "# Article\n";
  write "input/articles/keep.md" "# Retained article\n";
  write "input/contents/guide.md" "# Guide\n";
  write "input/subux.json" "{\"baseUrl\":\"https://example.com\"}\n";
  write "input/public/favicons/icon.png" "\x89PNG\000";
  write "input/public/_headers" "/sw.js\n  Cache-Control: no-store\n";
  List.iter (fun mode ->
    let static = "static-" ^ mode and output = "cf-" ^ mode in
    build ~expect_success:true [ "--mode"; mode; "-o"; path static ];
    build ~expect_success:true
      ([ "--mode"; mode; "--name"; "my-subux" ] @ cloudflare output);
    compare_trees (path static)
      (path (output ^ "/.cloudflare/output/v0/workers/default/assets")))
    [ "blog"; "docs"; "unified" ];

  present (asset "favicons/icon.png");
  ensure (read_file (path (asset "_headers")) |> String.ends_with
            ~suffix:"\n/sw.js\n  Cache-Control: no-store\n")
    "Cloudflare assets merge public _headers";

  (* Deleted and newly drafted pages disappear; unrelated files survive. *)
  write "cf-blog/keep.txt" "keep\n";
  present (asset "post/index.html");
  Unix.unlink (path "input/articles/post.md");
  build ~expect_success:true (cloudflare "cf-blog/");
  absent (asset "post/index.html");
  ensure (read_file (path "cf-blog/keep.txt") = "keep\n") "Unrelated output was changed";
  write "input/articles/published.md" "# Published\n";
  build ~expect_success:true (cloudflare "cf-blog");
  present (asset "published/index.html");
  write "input/articles/published.md" "---\ndraft: true\n---\n# Draft\n";
  build ~expect_success:true (cloudflare "cf-blog");
  absent (asset "published/index.html");

  (* Check and invalid arguments must not create output. *)
  invoke ~expect_success:true
    [ "check"; "-i"; path "input"; "--tokens"; path "토큰 파일.json";
      "--adapter"; "cloudflare"; "-o"; path "checked" ];
  absent "checked";
  List.iter (build ~expect_success:false)
    [ [ "--adapter"; "typo"; "-o"; path "invalid" ];
      cloudflare "invalid" @ [ "--name"; "../private" ];
      cloudflare "invalid" @ [ "--compatibility-date"; "2026-02-29" ];
      [ "--adapter" ]; [ "--name" ]; [ "--compatibility-date" ] ];
  absent "invalid";

  (* A rejected source preserves every byte of the previous build. *)
  let snapshot = List.map (fun file -> file, read_file (path (assets ^ file)))
      (files (path assets) "") in
  write "input/articles/unsafe.md" "---\ntitle: !!js/function unsafe\n---\n";
  build ~expect_success:false (cloudflare "cf-blog");
  ensure (List.map fst snapshot = files (path assets) "") "Failed build changed file list";
  List.iter (fun (file, content) ->
    ensure (read_file (path (assets ^ file)) = content)
      ("Failed build changed output: " ^ file)) snapshot;
  Unix.unlink (path "input/articles/unsafe.md");

  (* Preserve unowned output and refuse symlinks without following them. *)
  write "unowned/.cloudflare/output/v0/keep.txt" "keep\n";
  build ~expect_success:false (cloudflare "unowned");
  ensure (read_file (path "unowned/.cloudflare/output/v0/keep.txt") = "keep\n")
    "Unowned output was changed";
  mkdir_p (path "outside");
  Unix.symlink (path "outside") (path "linked");
  List.iter (fun output -> build ~expect_success:false (cloudflare output))
    [ "linked"; "linked/"; "linked/child" ];
  mkdir_p (path "nested");
  Unix.symlink (path "outside") (path "nested/.cloudflare");
  build ~expect_success:false (cloudflare "nested");
  ensure (Sys.readdir (path "outside") = [||]) "Wrote through an output symlink";
  let output = path "cf-blog/.cloudflare/output" in
  ensure (Array.to_list (Sys.readdir output) = [ "v0" ])
    "Staging directories survived a build"

let run ~executable ~tokens_path =
  try
    let executable = Unix.realpath executable and tokens_path = Unix.realpath tokens_path in
    let work = Filename.temp_file
        ~temp_dir:(Unix.realpath (Filename.get_temp_dir_name ())) "subux cli " "" in
    Unix.unlink work;
    Unix.mkdir work 0o700;
    Fun.protect ~finally:(fun () -> remove_tree work) (fun () ->
      test_workspace ~executable ~tokens_path work);
    print_endline "PASS: OCaml Cloudflare CLI builds, rebuilds, validation and output protection"
  with exn ->
    prerr_endline ("FAIL: Cloudflare CLI integration: " ^ Printexc.to_string exn);
    exit 1
