open Domain
open Domain.Result_syntax

type value =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of value list
  | Object of (string * value) list

let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let rec skip_ws s len i =
  if i < len && is_ws s.[i] then skip_ws s len (i + 1) else i

let hex_val c =
  if c >= '0' && c <= '9' then Some (Char.code c - Char.code '0')
  else if c >= 'a' && c <= 'f' then Some (Char.code c - Char.code 'a' + 10)
  else if c >= 'A' && c <= 'F' then Some (Char.code c - Char.code 'A' + 10)
  else None

let parse_hex4 s len i =
  if i + 4 > len then Error (Invalid_json "Unexpected end in \\u escape")
  else
    match
      (hex_val s.[i], hex_val s.[i + 1], hex_val s.[i + 2], hex_val s.[i + 3])
    with
    | Some a, Some b, Some c, Some d ->
        Ok ((a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d, i + 4)
    | _ -> Error (Invalid_json "Invalid hex digit in \\u escape")

let parse_string s len start_quote =
  let buf = Buffer.create 32 in
  let rec loop i =
    if i >= len then Error (Invalid_json "Unterminated string literal")
    else
      match s.[i] with
      | '"' -> Ok (Buffer.contents buf, i + 1)
      | '\\' ->
          if i + 1 >= len then Error (Invalid_json "Unterminated escape")
          else (
            match s.[i + 1] with
            | '"' ->
                Buffer.add_char buf '"';
                loop (i + 2)
            | '\\' ->
                Buffer.add_char buf '\\';
                loop (i + 2)
            | '/' ->
                Buffer.add_char buf '/';
                loop (i + 2)
            | 'b' ->
                Buffer.add_char buf '\b';
                loop (i + 2)
            | 'f' ->
                Buffer.add_char buf '\012';
                loop (i + 2)
            | 'n' ->
                Buffer.add_char buf '\n';
                loop (i + 2)
            | 'r' ->
                Buffer.add_char buf '\r';
                loop (i + 2)
            | 't' ->
                Buffer.add_char buf '\t';
                loop (i + 2)
            | 'u' ->
                let* code, next_i = parse_hex4 s len (i + 2) in
                if Uchar.is_valid code then (
                  Buffer.add_utf_8_uchar buf (Uchar.of_int code);
                  loop next_i)
                else Error (Invalid_json "Invalid unicode scalar value")
            | c ->
                Error
                  (Invalid_json (Printf.sprintf "Invalid escape char: \\%c" c)))
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop (start_quote + 1)

let is_num_char = function
  | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
  | _ -> false

let parse_number s len start_i =
  let rec scan i =
    if i < len && is_num_char s.[i] then scan (i + 1) else i
  in
  let end_i = scan start_i in
  let sub = String.sub s start_i (end_i - start_i) in
  match float_of_string_opt sub with
  | Some f -> Ok (Number f, end_i)
  | None -> Error (Invalid_json ("Invalid number: " ^ sub))

let rec parse_value s len i =
  let i = skip_ws s len i in
  if i >= len then Error (Invalid_json "Unexpected end of JSON input")
  else
    match s.[i] with
    | '"' ->
        let+ str, next_i = parse_string s len i in
        (String str, next_i)
    | '{' -> parse_object s len (i + 1)
    | '[' -> parse_array s len (i + 1)
    | 't' ->
        if i + 4 <= len && String.sub s i 4 = "true" then Ok (Bool true, i + 4)
        else Error (Invalid_json "Expected 'true'")
    | 'f' ->
        if i + 5 <= len && String.sub s i 5 = "false" then
          Ok (Bool false, i + 5)
        else Error (Invalid_json "Expected 'false'")
    | 'n' ->
        if i + 4 <= len && String.sub s i 4 = "null" then Ok (Null, i + 4)
        else Error (Invalid_json "Expected 'null'")
    | c when (c >= '0' && c <= '9') || c = '-' -> parse_number s len i
    | c ->
        Error
          (Invalid_json
             (Printf.sprintf "Unexpected character '%c' at index %d" c i))

and parse_array s len i =
  let i = skip_ws s len i in
  if i < len && s.[i] = ']' then Ok (Array [], i + 1)
  else
    let rec loop acc idx =
      let* v, next_i = parse_value s len idx in
      let next_i = skip_ws s len next_i in
      if next_i >= len then Error (Invalid_json "Unterminated array")
      else
        match s.[next_i] with
        | ',' -> loop (v :: acc) (next_i + 1)
        | ']' -> Ok (Array (List.rev (v :: acc)), next_i + 1)
        | c ->
            Error
              (Invalid_json
                 (Printf.sprintf "Expected ',' or ']' in array, got '%c'" c))
    in
    loop [] i

and parse_object s len i =
  let i = skip_ws s len i in
  if i < len && s.[i] = '}' then Ok (Object [], i + 1)
  else
    let rec loop acc idx =
      let idx = skip_ws s len idx in
      if idx >= len || s.[idx] <> '"' then
        Error (Invalid_json "Expected string key in object")
      else
        let* key, after_key = parse_string s len idx in
        let colon_i = skip_ws s len after_key in
        if colon_i >= len || s.[colon_i] <> ':' then
          Error (Invalid_json ("Expected ':' after key " ^ key))
        else
          let* v, after_val = parse_value s len (colon_i + 1) in
          let next_i = skip_ws s len after_val in
          if next_i >= len then Error (Invalid_json "Unterminated object")
          else
            match s.[next_i] with
            | ',' -> loop ((key, v) :: acc) (next_i + 1)
            | '}' -> Ok (Object (List.rev ((key, v) :: acc)), next_i + 1)
            | c ->
                Error
                  (Invalid_json
                     (Printf.sprintf "Expected ',' or '}' in object, got '%c'" c))
    in
    loop [] i

let parse raw =
  let len = String.length raw in
  let start =
    if
      len >= 3
      && Char.code raw.[0] = 0xEF
      && Char.code raw.[1] = 0xBB
      && Char.code raw.[2] = 0xBF
    then 3
    else 0
  in
  let* v, end_i = parse_value raw len start in
  let final_i = skip_ws raw len end_i in
  if final_i = len then Ok v
  else Error (Invalid_json "Trailing content after root JSON value")

let escape_json_string buf s =
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let rec write_value buf = function
  | Null -> Buffer.add_string buf "null"
  | Bool true -> Buffer.add_string buf "true"
  | Bool false -> Buffer.add_string buf "false"
  | Number f ->
      if Float.is_integer f then
        Buffer.add_string buf (Int.to_string (Float.to_int f))
      else Buffer.add_string buf (Printf.sprintf "%g" f)
  | String s -> escape_json_string buf s
  | Array items ->
      Buffer.add_char buf '[';
      List.iteri
        (fun idx item ->
          if idx > 0 then Buffer.add_char buf ',';
          write_value buf item)
        items;
      Buffer.add_char buf ']'
  | Object fields ->
      Buffer.add_char buf '{';
      List.iteri
        (fun idx (k, v) ->
          if idx > 0 then Buffer.add_char buf ',';
          escape_json_string buf k;
          Buffer.add_char buf ':';
          write_value buf v)
        fields;
      Buffer.add_char buf '}'

let to_string v =
  let buf = Buffer.create 256 in
  write_value buf v;
  Buffer.contents buf
