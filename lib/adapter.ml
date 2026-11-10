open Domain

type t = Static | Cloudflare

let of_string = function
  | "static" -> Ok Static
  | "cloudflare" -> Ok Cloudflare
  | name -> Error (Config_error ("Unsupported adapter: " ^ name))

let cloudflare_output_dir = ".cloudflare/output/v0"

let valid_name name =
  let alnum = function 'a' .. 'z' | '0' .. '9' -> true | _ -> false in
  let len = String.length name in
  len > 0 && len <= 63 && alnum name.[0] && alnum name.[len - 1]
  && String.for_all (fun c -> alnum c || c = '-') name
  && not (String.for_all (function '0' .. '9' -> true | _ -> false) name)

let valid_date raw =
  match parse_date raw with
  | Some date when format_iso date = raw ->
      let leap = date.year mod 4 = 0
                 && (date.year mod 100 <> 0 || date.year mod 400 = 0) in
      let days = match date.month with
        | 2 -> if leap then 29 else 28
        | 4 | 6 | 9 | 11 -> 30
        | _ -> 31
      in
      date.day <= days
  | Some _ | None -> false

let valid_path path =
  String.for_all (fun c -> c > '\031' && c <> '\127' && c <> '\\') path
  && List.for_all
       (fun part -> part <> "" && part <> "." && part <> "..")
       (String.split_on_char '/' path)

(* Sorted paths place a file before anything nested beneath it, so one pass
   finds duplicates and file/directory conflicts. *)
let check_paths ~unsafe ~conflict paths =
  let rec check = function
    | [] -> Ok ()
    | path :: rest ->
        if not (valid_path path) then Error (Config_error (unsafe ^ path))
        else if List.exists
            (fun other -> other = path
              || String.starts_with ~prefix:(path ^ "/") other) rest then
          Error (Config_error (conflict ^ path))
        else check rest
  in
  check (List.sort String.compare paths)

let out_paths artifacts =
  List.map (fun (a : Site_builder.output_artifact) -> a.out_path) artifacts

let with_static_files ~generated static_files =
  let open Result_syntax in
  let static_paths = out_paths static_files in
  let* () =
    check_paths ~unsafe:"Unsafe static file path: "
      ~conflict:"Static file conflicts with another static file: " static_paths
  in
  (* Only pairs involving a static file are judged here; generated output keeps
     whatever layout the selected adapter accepts. *)
  let nests a b = String.starts_with ~prefix:(a ^ "/") b in
  let generated_paths = out_paths generated in
  match
    List.find_opt
      (fun s -> List.exists (fun g -> g = s || nests g s || nests s g)
          generated_paths)
      static_paths
  with
  | Some path ->
      Error (Config_error ("Static file conflicts with generated output: " ^ path))
  | None -> Ok (generated @ static_files)

let validate_artifacts artifacts =
  let paths = out_paths artifacts in
  let open Result_syntax in
  let* () =
    check_paths ~unsafe:"Unsafe adapter artifact path: "
      ~conflict:"Conflicting adapter artifact path: " paths
  in
  if List.mem "index.html" paths && List.mem "404.html" paths then Ok ()
  else Error (Config_error "Cloudflare output requires index.html and 404.html")

let adapt adapter ~name ~compatibility_date artifacts =
  match adapter with
  | Static -> Ok artifacts
  | Cloudflare ->
      let open Result_syntax in
      let* () =
        if valid_name name then Ok ()
        else Error (Config_error
          "Cloudflare name must be 1-63 lowercase letters, digits or hyphens, with alphanumeric ends, and not all digits")
      in
      let* () =
        if valid_date compatibility_date then Ok ()
        else Error (Config_error "Cloudflare compatibility date must be a valid YYYY-MM-DD date")
      in
      let* () = validate_artifacts artifacts in
      let open Json_parser in
      let artifact path json : Site_builder.output_artifact =
        { out_path = cloudflare_output_dir ^ "/" ^ path;
          content = to_string json ^ "\n" }
      in
      let root_config = artifact "config.json"
          (Object [ "buildContext", Object [ "isPreview", Bool false ] ]) in
      let worker_config = artifact "workers/default/worker.config.json"
          (Object [
            "name", String name;
            "compatibilityDate", String compatibility_date;
            "workersDev", Bool true;
            "assets", Object [
              "htmlHandling", String "drop-trailing-slash";
              "notFoundHandling", String "404-page";
            ];
          ]) in
      Ok (root_config :: worker_config :: List.map
        (fun (a : Site_builder.output_artifact) ->
          { a with out_path = cloudflare_output_dir
                    ^ "/workers/default/assets/" ^ a.out_path }) artifacts)
