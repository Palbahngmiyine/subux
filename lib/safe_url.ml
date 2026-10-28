let is_c0_or_space c = Char.code c <= 0x20

let trim_c0_and_space s =
  let len = String.length s in
  let rec find_start i =
    if i < len && is_c0_or_space s.[i] then find_start (i + 1) else i
  in
  let rec find_end i =
    if i >= 0 && is_c0_or_space s.[i] then find_end (i - 1) else i
  in
  let start_idx = find_start 0 in
  let end_idx = find_end (len - 1) in
  if start_idx > end_idx then ""
  else String.sub s start_idx (end_idx - start_idx + 1)

let decode_numeric_entities s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if i + 3 < len && s.[i] = '&' && s.[i + 1] = '#' then
      let rec find_semi j =
        if j >= len || j - i > 10 then None
        else if s.[j] = ';' then Some j
        else find_semi (j + 1)
      in
      match find_semi (i + 2) with
      | Some semi_idx ->
          let body = String.sub s (i + 2) (semi_idx - (i + 2)) in
          let code_opt =
            if
              String.length body > 1
              && (body.[0] = 'x' || body.[0] = 'X')
            then
              int_of_string_opt ("0" ^ body)
            else int_of_string_opt body
          in
          (match code_opt with
          | Some code when code >= 0 && code <= 0x7f ->
              Buffer.add_char buf (Char.chr code);
              loop (semi_idx + 1)
          | _ ->
              Buffer.add_char buf s.[i];
              loop (i + 1))
      | None ->
          Buffer.add_char buf s.[i];
          loop (i + 1)
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let remove_ascii_tab_lf_cr s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | '\t' | '\n' | '\r' -> ()
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_scheme_char c =
  is_alpha c || (c >= '0' && c <= '9') || c = '+' || c = '-' || c = '.'

let extract_scheme s =
  let len = String.length s in
  if len = 0 || not (is_alpha s.[0]) then None
  else
    let rec scan i =
      if i >= len then None
      else if s.[i] = ':' then
        Some (String.lowercase_ascii (String.sub s 0 i))
      else if is_scheme_char s.[i] then scan (i + 1)
      else None
    in
    scan 1

let is_safe_url ~allow_contact ~decode_entities raw =
  let step1 = if decode_entities then decode_numeric_entities raw else raw in
  let step2 = trim_c0_and_space step1 in
  let normalized = remove_ascii_tab_lf_cr step2 in
  if normalized = "" then false
  else
    match extract_scheme normalized with
    | None ->
        (* Relative path, fragment (#...), or network-path (//...) *)
        true
    | Some ("https" | "http") -> true
    | Some ("mailto" | "tel") -> allow_contact
    | Some _ -> false

let sanitize_url ~allow_contact ~decode_entities raw =
  if is_safe_url ~allow_contact ~decode_entities raw then Some raw else None

let is_hex_digit c =
  (c >= '0' && c <= '9')
  || (c >= 'a' && c <= 'f')
  || (c >= 'A' && c <= 'F')

let encode_uri_preserving_pct s =
  let len = String.length s in
  let buf = Buffer.create (len + 8) in
  let is_unreserved_or_uri_punct c =
    is_alpha c
    || (c >= '0' && c <= '9')
    || String.contains "-_.!~*'();/?:@&=+$,#" c
  in
  let rec loop i =
    if i >= len then ()
    else if
      s.[i] = '%'
      && i + 2 < len
      && is_hex_digit s.[i + 1]
      && is_hex_digit s.[i + 2]
    then (
      Buffer.add_char buf '%';
      Buffer.add_char buf s.[i + 1];
      Buffer.add_char buf s.[i + 2];
      loop (i + 3))
    else
      let c = s.[i] in
      if is_unreserved_or_uri_punct c then (
        Buffer.add_char buf c;
        loop (i + 1))
      else (
        Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c));
        loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let wiki_link_href permalink =
  let resolved =
    String.map (fun c -> if c = ' ' then '-' else c)
      (String.lowercase_ascii permalink)
  in
  let len = String.length resolved in
  let rec skip_leading_slashes_and_controls i =
    if
      i < len
      && (resolved.[i] = '/'
         || resolved.[i] = '\\'
         || Char.code resolved.[i] <= 0x20)
    then skip_leading_slashes_and_controls (i + 1)
    else i
  in
  let start = skip_leading_slashes_and_controls 0 in
  let stripped = String.sub resolved start (len - start) in
  let forward_slashes =
    String.map (fun c -> if c = '\\' then '/' else c) stripped
  in
  (* Also collapse any leading slashes that emerged after stripping control chars *)
  let flen = String.length forward_slashes in
  let rec skip_slashes i =
    if i < flen && forward_slashes.[i] = '/' then skip_slashes (i + 1) else i
  in
  let s2 = skip_slashes 0 in
  let clean_path = String.sub forward_slashes s2 (flen - s2) in
  "/" ^ encode_uri_preserving_pct clean_path
