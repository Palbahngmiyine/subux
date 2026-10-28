open Cmarkit
module C = Cmarkit_renderer.Context
module String_set = Set.Make (String)
module String_map = Map.Make (String)

type rendered_markdown = {
  html : string;
  toc : Domain.toc_item list;
  plain_text : string;
  has_mermaid : bool;
}

type Inline.t +=
  | Wiki_link of { href : string; label : string }

type Block.t +=
  | Callout_block of {
      kind : Domain.callout_kind;
      expandable : bool;
      expanded : bool;
      title : Inline.t;
      body : Block.t list;
    }
  | Video_block of {
      src : string option;
      title : string option;
    }

(* 1. Pure string and HTML escaping helpers *)
let escape_html_text (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create (len + 16) in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else begin
      begin match s.[i] with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\x00' -> Buffer.add_utf_8_uchar buf Uchar.rep
      | c -> Buffer.add_char buf c
      end;
      loop (i + 1)
    end
  in
  loop 0

let escape_code_text (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create (len + 16) in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else begin
      begin match s.[i] with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&#x3C;"
      | '\x00' -> Buffer.add_utf_8_uchar buf Uchar.rep
      | c -> Buffer.add_char buf c
      end;
      loop (i + 1)
    end
  in
  loop 0

let pct_encode_url (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create (len + 16) in
  let hex_digit n =
    if n < 10 then Char.chr (Char.code '0' + n)
    else Char.chr (Char.code 'A' + (n - 10))
  in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else begin
      let c = s.[i] in
      begin match c with
      | '%'
      | 'A' .. 'Z'
      | 'a' .. 'z'
      | '0' .. '9'
      | '-' | '.' | '_' | '~'
      | '!' | '$' | '(' | ')' | '*' | '+' | ',' | ';' | '='
      | ':' | '/' | '?' | '#' | '@' ->
          Buffer.add_char buf c
      | '&' -> Buffer.add_string buf "&amp;"
      | '\'' -> Buffer.add_string buf "&#x27;"
      | _ ->
          let code = Char.code c in
          Buffer.add_char buf '%';
          Buffer.add_char buf (hex_digit ((code lsr 4) land 0xF));
          Buffer.add_char buf (hex_digit (code land 0xF))
      end;
      loop (i + 1)
    end
  in
  loop 0

(* 2. WikiLink parsing inside Inline.Text *)
let find_substring_from (haystack : string) ( start : int ) (needle : string) : int option =
  let hlen = String.length haystack in
  let nlen = String.length needle in
  if nlen = 0 then Some start
  else
    let rec loop i =
      if i + nlen > hlen then None
      else if String.sub haystack i nlen = needle then Some i
      else loop (i + 1)
    in
    loop start

let parse_wiki_links_in_text (s : string) (meta : Meta.t) : Inline.t list =
  let len = String.length s in
  let rec loop pos acc =
    if pos >= len then List.rev acc
    else
      match find_substring_from s pos "[[" with
      | None ->
          let rest = String.sub s pos (len - pos) in
          List.rev (Inline.Text (rest, meta) :: acc)
      | Some open_idx ->
          let before_acc =
            if open_idx > pos then
              Inline.Text (String.sub s pos (open_idx - pos), meta) :: acc
            else acc
          in
          let content_start = open_idx + 2 in
          match find_substring_from s content_start "]]" with
          | None ->
              let rest = String.sub s open_idx (len - open_idx) in
              List.rev (Inline.Text (rest, meta) :: before_acc)
          | Some close_idx ->
              let inner = String.sub s content_start (close_idx - content_start) in
              if inner = "" || String.contains inner '\n' then
                let raw = String.sub s open_idx (close_idx + 2 - open_idx) in
                loop (close_idx + 2) (Inline.Text (raw, meta) :: before_acc)
              else
                let target, label =
                  match String.index_opt inner '|' with
                  | None -> (inner, inner)
                  | Some bar_idx ->
                      let t = String.sub inner 0 bar_idx in
                      let l =
                        String.sub inner (bar_idx + 1)
                          (String.length inner - bar_idx - 1)
                      in
                      (t, if l = "" then t else l)
                in
                let href = Safe_url.wiki_link_href target in
                loop (close_idx + 2) (Wiki_link { href; label } :: before_acc)
  in
  loop 0 []

(* 3. Video Leaf Directive parsing: ::video{src="..." title="..."} *)
let extract_attr (attrs_str : string) (attr_name : string) : string option =
  let prefix = attr_name ^ "=\"" in
  match find_substring_from attrs_str 0 prefix with
  | None -> None
  | Some idx ->
      let v_start = idx + String.length prefix in
      let len = String.length attrs_str in
      let rec find_end i =
        if i >= len then None
        else if attrs_str.[i] = '"' then
          Some (String.sub attrs_str v_start (i - v_start))
        else find_end (i + 1)
      in
      find_end v_start

let try_parse_video_directive (raw_text : string) : Block.t option =
  let s = String.trim raw_text in
  let len = String.length s in
  if len >= 9 && String.starts_with ~prefix:"::video{" s && s.[len - 1] = '}' then
    let attrs = String.sub s 8 (len - 9) in
    match extract_attr attrs "src" with
    | None | Some "" -> None
    | Some raw_src ->
        let safe_src =
          Safe_url.sanitize_url ~allow_contact:false ~decode_entities:true raw_src
        in
        let title = extract_attr attrs "title" in
        Some (Video_block { src = safe_src; title })
  else None

(* 4. Obsidian Callout transformation *)
let is_callout_type_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let parse_callout_marker (s : string) : (Domain.callout_kind * bool * bool * string) option =
  let len = String.length s in
  if len < 4 || s.[0] <> '[' || s.[1] <> '!' then None
  else
    let rec find_close i =
      if i >= len then None
      else if s.[i] = ']' then Some i
      else if is_callout_type_char s.[i] then find_close (i + 1)
      else None
    in
    match find_close 2 with
    | None -> None
    | Some close_idx when close_idx = 2 -> None
    | Some close_idx ->
        let raw_type = String.sub s 2 (close_idx - 2) in
        let kind = Domain.callout_kind_of_string raw_type in
        let after_bracket = close_idx + 1 in
        let expandable, expanded, after_fold =
          if after_bracket < len && s.[after_bracket] = '+' then
            (true, true, after_bracket + 1)
          else if after_bracket < len && s.[after_bracket] = '-' then
            (true, false, after_bracket + 1)
          else (false, false, after_bracket)
        in
        let rec skip_spaces i =
          if i < len && (s.[i] = ' ' || s.[i] = '\t') then skip_spaces (i + 1)
          else i
        in
        let rest_start = skip_spaces after_fold in
        let remainder = String.sub s rest_start (len - rest_start) in
        Some (kind, expandable, expanded, remainder)

let rec flatten_top_inlines (inl : Inline.t) : Inline.t list =
  match inl with
  | Inline.Inlines (xs, _) -> List.concat_map flatten_top_inlines xs
  | other -> [ other ]

let rec split_inlines_at_break (nodes : Inline.t list) :
    Inline.t list * Inline.t list * bool =
  match nodes with
  | [] -> ([], [], false)
  | Inline.Break _ :: rest -> ([], rest, true)
  | Inline.Inlines (children, _) :: rest ->
      split_inlines_at_break (children @ rest)
  | Inline.Emphasis (e, m) :: rest ->
      let child = Inline.Emphasis.inline e in
      let c_before, c_after, found = split_inlines_at_break [ child ] in
      if found then
        let delim = Inline.Emphasis.delim e in
        let b_node =
          match c_before with
          | [] -> []
          | _ ->
              [
                Inline.Emphasis
                  (Inline.Emphasis.make ~delim (Inline.Inlines (c_before, m)), m);
              ]
        in
        let a_node =
          match c_after with
          | [] -> []
          | _ ->
              [
                Inline.Emphasis
                  (Inline.Emphasis.make ~delim (Inline.Inlines (c_after, m)), m);
              ]
        in
        (b_node, a_node @ rest, true)
      else
        let r_before, r_after, r_found = split_inlines_at_break rest in
        (Inline.Emphasis (e, m) :: r_before, r_after, r_found)
  | Inline.Strong_emphasis (e, m) :: rest ->
      let child = Inline.Emphasis.inline e in
      let c_before, c_after, found = split_inlines_at_break [ child ] in
      if found then
        let delim = Inline.Emphasis.delim e in
        let b_node =
          match c_before with
          | [] -> []
          | _ ->
              [
                Inline.Strong_emphasis
                  (Inline.Emphasis.make ~delim (Inline.Inlines (c_before, m)), m);
              ]
        in
        let a_node =
          match c_after with
          | [] -> []
          | _ ->
              [
                Inline.Strong_emphasis
                  (Inline.Emphasis.make ~delim (Inline.Inlines (c_after, m)), m);
              ]
        in
        (b_node, a_node @ rest, true)
      else
        let r_before, r_after, r_found = split_inlines_at_break rest in
        (Inline.Strong_emphasis (e, m) :: r_before, r_after, r_found)
  | node :: rest ->
      let r_before, r_after, r_found = split_inlines_at_break rest in
      (node :: r_before, r_after, r_found)

let rec flatten_blocks (b : Block.t) : Block.t list =
  match b with
  | Block.Blocks (bs, _) -> List.concat_map flatten_blocks bs
  | Block.Blank_line _ -> []
  | other -> [ other ]

let try_transform_callout (bq : Block.Block_quote.t) : Block.t option =
  let blocks = flatten_blocks (Block.Block_quote.block bq) in
  match blocks with
  | Block.Paragraph (first_para, p_meta) :: rest_blocks ->
      let inlines = flatten_top_inlines (Block.Paragraph.inline first_para) in
      begin match inlines with
      | Inline.Text (first_text, t_meta) :: rest_inlines ->
          begin match parse_callout_marker first_text with
          | None -> None
          | Some (kind, expandable, expanded, rem_text) ->
              let adjusted_inlines =
                if rem_text = "" then rest_inlines
                else Inline.Text (rem_text, t_meta) :: rest_inlines
              in
              let title_nodes, body_inline_nodes, _ =
                split_inlines_at_break adjusted_inlines
              in
              let title_inline =
                match title_nodes with
                | [] ->
                    Inline.Text (Domain.callout_kind_to_string kind, t_meta)
                | [ single ] -> single
                | many -> Inline.Inlines (many, p_meta)
              in
              let body_blocks =
                match body_inline_nodes with
                | [] -> rest_blocks
                | many ->
                    let body_para =
                      Block.Paragraph.make (Inline.Inlines (many, p_meta))
                    in
                    Block.Paragraph (body_para, p_meta) :: rest_blocks
              in
              Some
                (Callout_block
                   {
                     kind;
                     expandable;
                     expanded;
                     title = title_inline;
                     body = body_blocks;
                   })
          end
      | _ -> None
      end
  | _ -> None

(* 5. Pure Functional Syntax Highlighter (--sx-color-code and .hljs classes) *)
let is_plain_text_lang (lang : string) : bool =
  match String.lowercase_ascii lang with
  | "text" | "txt" | "plain" | "mermaid" | "mmd" -> true
  | _ -> false

let is_ident_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_ident_cont = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_keyword (lang : string) (word : string) : bool =
  match lang with
  | "sh" | "bash" | "shell" | "zsh" ->
      List.mem word
        [
          "if"; "then"; "else"; "elif"; "fi"; "for"; "in"; "do"; "done";
          "while"; "until"; "case"; "esac"; "function"; "return"; "local";
          "export"; "set"; "unset"; "readonly";
        ]
  | "json" -> false
  | _ ->
      List.mem word
        [
          "const"; "let"; "var"; "function"; "return"; "if"; "else"; "for";
          "while"; "match"; "with"; "type"; "module"; "sig"; "struct"; "open";
          "import"; "from"; "export"; "default"; "async"; "await"; "class";
          "interface"; "extends"; "implements"; "new"; "try"; "catch"; "throw";
          "fn"; "pub"; "mut"; "impl"; "trait"; "enum"; "use"; "def"; "lambda";
          "yield"; "package"; "func"; "defer"; "go"; "select"; "when"; "begin";
          "end"; "rec"; "and"; "of"; "val"; "external";
        ]

let is_literal (word : string) : bool =
  List.mem word [ "true"; "false"; "null"; "undefined"; "None"; "Some"; "nil" ]

let is_builtin (lang : string) (word : string) : bool =
  match lang with
  | "sh" | "bash" | "shell" | "zsh" ->
      List.mem word [ "echo"; "cd"; "pwd"; "exit"; "printf"; "read"; "test" ]
  | _ -> false

let highlight_yaml_line (line : string) : string =
  let len = String.length line in
  let trimmed = String.trim line in
  if String.starts_with ~prefix:"#" trimmed then
    "<span class=\"hljs-comment\">" ^ escape_code_text line ^ "</span>"
  else
    match String.index_opt line ':' with
    | Some colon_idx when colon_idx > 0 ->
        let key_part = String.sub line 0 (colon_idx + 1) in
        let val_part = String.sub line (colon_idx + 1) (len - colon_idx - 1) in
        let trimmed_val = String.trim val_part in
        let rendered_val =
          if trimmed_val = "" then escape_code_text val_part
          else if trimmed_val = "true" || trimmed_val = "false" || trimmed_val = "null" then
            " <span class=\"hljs-literal\">" ^ escape_code_text trimmed_val ^ "</span>"
          else
            " <span class=\"hljs-string\">" ^ escape_code_text trimmed_val ^ "</span>"
        in
        "<span class=\"hljs-attr\">" ^ escape_code_text key_part ^ "</span>" ^ rendered_val
    | _ -> escape_code_text line

let highlight_code_line (lang : string) (line : string) : string =
  let norm_lang = String.lowercase_ascii lang in
  if norm_lang = "yml" || norm_lang = "yaml" then highlight_yaml_line line
  else if norm_lang = "diff" || norm_lang = "patch" then
    if String.starts_with ~prefix:"+" line then
      "<span class=\"hljs-addition\">" ^ escape_code_text line ^ "</span>"
    else if String.starts_with ~prefix:"-" line then
      "<span class=\"hljs-deletion\">" ^ escape_code_text line ^ "</span>"
    else escape_code_text line
  else
    let len = String.length line in
    let buf = Buffer.create (len + 32) in
    let rec loop i =
      if i >= len then Buffer.contents buf
      else
        let c = line.[i] in
        if c = '#' && (norm_lang = "sh" || norm_lang = "bash" || norm_lang = "shell" || norm_lang = "zsh" || norm_lang = "py" || norm_lang = "python") then begin
          Buffer.add_string buf "<span class=\"hljs-comment\">";
          Buffer.add_string buf (escape_code_text (String.sub line i (len - i)));
          Buffer.add_string buf "</span>";
          Buffer.contents buf
        end
        else if c = '/' && i + 1 < len && line.[i + 1] = '/' then begin
          Buffer.add_string buf "<span class=\"hljs-comment\">";
          Buffer.add_string buf (escape_code_text (String.sub line i (len - i)));
          Buffer.add_string buf "</span>";
          Buffer.contents buf
        end
        else if c = '"' || c = '\'' then
          let quote = c in
          let rec find_close j =
            if j >= len then len - 1
            else if line.[j] = '\\' && j + 1 < len then find_close (j + 2)
            else if line.[j] = quote then j
            else find_close (j + 1)
          in
          let close_idx = find_close (i + 1) in
          let str_token = String.sub line i (close_idx - i + 1) in
          let after_quote = close_idx + 1 in
          let is_json_attr =
            norm_lang = "json"
            &&
            let rec check_colon k =
              if k >= len then false
              else if line.[k] = ' ' || line.[k] = '\t' then check_colon (k + 1)
              else line.[k] = ':'
            in
            check_colon after_quote
          in
          let span_class = if is_json_attr then "hljs-attr" else "hljs-string" in
          Buffer.add_string buf ("<span class=\"" ^ span_class ^ "\">");
          Buffer.add_string buf (escape_code_text str_token);
          Buffer.add_string buf "</span>";
          loop after_quote
        else if is_digit c then
          let rec find_num_end j =
            if j < len && (is_digit line.[j] || line.[j] = '.' || line.[j] = 'x' || line.[j] = 'X') then
              find_num_end (j + 1)
            else j
          in
          let num_end = find_num_end (i + 1) in
          let num_tok = String.sub line i (num_end - i) in
          Buffer.add_string buf "<span class=\"hljs-number\">";
          Buffer.add_string buf (escape_code_text num_tok);
          Buffer.add_string buf "</span>";
          loop num_end
        else if is_ident_start c then begin
          let rec find_id_end j =
            if j < len && is_ident_cont line.[j] then find_id_end (j + 1) else j
          in
          let id_end = find_id_end (i + 1) in
          let word = String.sub line i (id_end - i) in
          if is_keyword norm_lang word then begin
            Buffer.add_string buf "<span class=\"hljs-keyword\">";
            Buffer.add_string buf (escape_code_text word);
            Buffer.add_string buf "</span>"
          end
          else if is_literal word then begin
            Buffer.add_string buf "<span class=\"hljs-literal\">";
            Buffer.add_string buf (escape_code_text word);
            Buffer.add_string buf "</span>"
          end
          else if is_builtin norm_lang word then begin
            Buffer.add_string buf "<span class=\"hljs-built_in\">";
            Buffer.add_string buf (escape_code_text word);
            Buffer.add_string buf "</span>"
          end
          else Buffer.add_string buf (escape_code_text word);
          loop id_end
        end
        else begin
          Buffer.add_string buf (escape_code_text (String.make 1 c));
          loop (i + 1)
        end
    in
    loop 0

(* 6. Plain-text extraction for Footnote tooltips, headings, and search index *)
let rec inline_to_plain_string (inl : Inline.t) : string =
  match inl with
  | Wiki_link { label; _ } -> label
  | Inline.Text (t, _) -> t
  | Inline.Code_span (cs, _) -> Inline.Code_span.code cs
  | Inline.Emphasis (e, _) | Inline.Strong_emphasis (e, _) ->
      inline_to_plain_string (Inline.Emphasis.inline e)
  | Inline.Ext_strikethrough (s, _) ->
      inline_to_plain_string (Inline.Strikethrough.inline s)
  | Inline.Link (l, _) -> inline_to_plain_string (Inline.Link.text l)
  | Inline.Image (i, _) -> inline_to_plain_string (Inline.Link.text i)
  | Inline.Autolink (a, _) -> fst (Inline.Autolink.link a)
  | Inline.Break _ -> " "
  | Inline.Inlines (is, _) ->
      String.concat "" (List.map inline_to_plain_string is)
  | Inline.Raw_html _ | Inline.Ext_math_span _ -> ""
  | _ ->
      let lines = Inline.to_plain_text ~break_on_soft:false inl in
      String.concat " " (List.map (String.concat "") lines)

let rec block_to_plain_string (blk : Block.t) : string =
  match blk with
  | Block.Paragraph (p, _) -> inline_to_plain_string (Block.Paragraph.inline p)
  | Block.Heading (h, _) -> inline_to_plain_string (Block.Heading.inline h)
  | Block.Block_quote (bq, _) ->
      block_to_plain_string (Block.Block_quote.block bq)
  | Block.Blocks (bs, _) ->
      bs
      |> List.map block_to_plain_string
      |> List.filter (fun s -> String.trim s <> "")
      |> String.concat " "
  | Block.List (l, _) ->
      Block.List'.items l
      |> List.map (fun (it, _) -> block_to_plain_string (Block.List_item.block it))
      |> List.filter (fun s -> String.trim s <> "")
      |> String.concat " "
  | Block.Code_block (cb, _) ->
      Block.Code_block.code cb
      |> List.map fst
      |> String.concat "\n"
  | Callout_block { title; body; _ } ->
      let t_str = inline_to_plain_string title in
      let b_str =
        body
        |> List.map block_to_plain_string
        |> List.filter (fun s -> String.trim s <> "")
        |> String.concat " "
      in
      if b_str = "" then t_str else t_str ^ " " ^ b_str
  | Video_block { title = Some t; _ } -> t
  | _ -> ""

(* 7. Pre-transform AST using Cmarkit.Mapper *)
let transform_doc (raw_md : string) (doc : Doc.t) : Doc.t =
  let raw_len = String.length raw_md in
  let inline_ext_default _m inl = Some inl in
  let block_ext_default m blk =
    match blk with
    | Callout_block cb ->
        let title = Option.value ~default:cb.title (Mapper.map_inline m cb.title) in
        let body = List.filter_map (Mapper.map_block m) cb.body in
        Some (Callout_block { cb with title; body })
    | Video_block _ as vb -> Some vb
    | other -> Some other
  in
  let inline _m = function
    | Inline.Text (s, meta) ->
        if String.contains s '[' then
          match parse_wiki_links_in_text s meta with
          | [ single ] -> Mapper.ret single
          | many -> Mapper.ret (Inline.Inlines (many, meta))
        else Mapper.default
    | _ -> Mapper.default
  in
  let block m = function
    | Block.Paragraph (p, meta) ->
        let loc = Meta.textloc meta in
        let first_b = Textloc.first_byte loc in
        let last_b = Textloc.last_byte loc in
        let raw_slice =
          if first_b >= 0 && last_b >= first_b && last_b < raw_len then
            String.sub raw_md first_b (last_b - first_b + 1)
          else inline_to_plain_string (Block.Paragraph.inline p)
        in
        begin match try_parse_video_directive raw_slice with
        | Some video_blk -> Mapper.ret video_blk
        | None ->
            begin match Mapper.map_inline m (Block.Paragraph.inline p) with
            | None -> Mapper.delete
            | Some inl' ->
                let p' =
                  Block.Paragraph.make
                    ~leading_indent:(Block.Paragraph.leading_indent p)
                    ~trailing_blanks:(Block.Paragraph.trailing_blanks p)
                    inl'
                in
                Mapper.ret (Block.Paragraph (p', meta))
            end
        end
    | Block.Block_quote (bq, meta) ->
        let inner_mapped =
          Option.value ~default:Block.empty
            (Mapper.map_block m (Block.Block_quote.block bq))
        in
        let bq' =
          Block.Block_quote.make
            ~indent:(Block.Block_quote.indent bq)
            inner_mapped
        in
        begin match try_transform_callout bq' with
        | Some callout_blk -> Mapper.ret callout_blk
        | None -> Mapper.ret (Block.Block_quote (bq', meta))
        end
    | _ -> Mapper.default
  in
  let mapper =
    Mapper.make ~inline_ext_default ~block_ext_default ~inline ~block ()
  in
  Mapper.map_doc mapper doc

(* 8. Custom HTML Renderer with Safe_url, Footnote Tooltips, TOC & Highlighting *)
type footnote_entry = {
  index : int;
  slug : string;
  ref_count : int;
  fn : Block.Footnote.t;
  plain : string;
}

type render_state = {
  mutable ids : String_set.t;
  mutable toc_rev : Domain.toc_item list;
  mutable has_mermaid : bool;
  mutable footnote_order : string list;
  mutable footnotes : footnote_entry String_map.t;
}

let rstate : render_state C.State.t = C.State.make ()

let init_render_state c _doc =
  C.State.set c rstate
    (Some
       {
         ids = String_set.of_list ["main-content"; "footnote-label"; "jt-search-modal";
           "jt-search-input"; "jt-search-results"; "jt-search-open"; "jt-theme-toggle"];
         toc_rev = [];
         has_mermaid = false;
         footnote_order = [];
         footnotes = String_map.empty;
       })

let unique_heading_id (st : render_state) (base_id : string) : string =
  let clean = if base_id = "" then "section" else base_id in
  let rec loop n =
    let candidate =
      if n = 0 then clean else clean ^ "-" ^ Int.to_string n
    in
    if String_set.mem candidate st.ids then loop (n + 1)
    else begin
      st.ids <- String_set.add candidate st.ids;
      candidate
    end
  in
  loop 0

let sanitize_footnote_slug (key : string) : string =
  let raw =
    if String.length key > 1 && key.[0] = '^' then
      String.sub key 1 (String.length key - 1)
    else key
  in
  String.map (function ' ' | '\t' -> '-' | c -> c) raw

let register_footnote_ref (st : render_state) (key : string)
    (fn : Block.Footnote.t) : footnote_entry =
  match String_map.find_opt key st.footnotes with
  | Some entry ->
      let updated = { entry with ref_count = entry.ref_count + 1 } in
      st.footnotes <- String_map.add key updated st.footnotes;
      updated
  | None ->
      let idx = List.length st.footnote_order + 1 in
      let slug = sanitize_footnote_slug key in
      let plain =
        String.trim (block_to_plain_string (Block.Footnote.block fn))
      in
      let entry = { index = idx; slug; ref_count = 1; fn; plain } in
      st.footnote_order <- st.footnote_order @ [ key ];
      st.footnotes <- String_map.add key entry st.footnotes;
      entry

let link_dest_and_title (ld : Link_definition.t) : string * string =
  let dest =
    match Link_definition.dest ld with
    | None -> ""
    | Some (u, _) -> u
  in
  let title =
    match Link_definition.title ld with
    | None -> ""
    | Some lines ->
        String.concat "\n" (List.map (fun (_, (t, _)) -> t) lines)
  in
  (dest, title)

let render_inline c = function
  | Wiki_link { href; label } ->
      C.string c "<a class=\"internal new\" href=\"";
      C.string c (escape_html_text href);
      C.string c "\">";
      C.string c (escape_html_text label);
      C.string c "</a>";
      true
  | Inline.Text (t, _) ->
      C.string c (escape_html_text t);
      true
  | Inline.Code_span (cs, _) ->
      C.string c "<code>";
      C.string c (escape_html_text (Inline.Code_span.code cs));
      C.string c "</code>";
      true
  | Inline.Emphasis (e, _) ->
      C.string c "<em>";
      C.inline c (Inline.Emphasis.inline e);
      C.string c "</em>";
      true
  | Inline.Strong_emphasis (e, _) ->
      C.string c "<strong>";
      C.inline c (Inline.Emphasis.inline e);
      C.string c "</strong>";
      true
  | Inline.Ext_strikethrough (s, _) ->
      C.string c "<del>";
      C.inline c (Inline.Strikethrough.inline s);
      C.string c "</del>";
      true
  | Inline.Break (b, _) ->
      begin match Inline.Break.type' b with
      | `Hard -> C.string c "<br>\n"
      | `Soft -> C.byte c '\n'
      end;
      true
  | Inline.Inlines (is, _) ->
      List.iter (C.inline c) is;
      true
  | Inline.Raw_html _ ->
      (* Disarm raw HTML completely *)
      true
  | Inline.Autolink (a, _) ->
      let pre = if Inline.Autolink.is_email a then "mailto:" else "" in
      let raw_url = pre ^ fst (Inline.Autolink.link a) in
      let encoded = pct_encode_url raw_url in
      if Safe_url.is_safe_url ~allow_contact:true ~decode_entities:false encoded then begin
        C.string c "<a href=\"";
        C.string c encoded;
        C.string c "\">";
        C.string c (escape_html_text (fst (Inline.Autolink.link a)));
        C.string c "</a>"
      end
      else begin
        C.string c "<a>";
        C.string c (escape_html_text (fst (Inline.Autolink.link a)));
        C.string c "</a>"
      end;
      true
  | Inline.Link (l, _) ->
      begin match Inline.Link.reference_definition (C.get_defs c) l with
      | Some (Link_definition.Def (ld, _)) ->
          let raw_dest, title = link_dest_and_title ld in
          let encoded_dest = pct_encode_url raw_dest in
          let is_safe =
            raw_dest <> ""
            && Safe_url.is_safe_url ~allow_contact:true ~decode_entities:false
                 encoded_dest
          in
          if is_safe then begin
            C.string c "<a href=\"";
            C.string c encoded_dest;
            C.byte c '"';
            if title <> "" then begin
              C.string c " title=\"";
              C.string c (escape_html_text title);
              C.byte c '"'
            end;
            C.byte c '>'
          end
          else C.string c "<a>";
          C.inline c (Inline.Link.text l);
          C.string c "</a>"
      | Some (Block.Footnote.Def (fn, _)) ->
          let st = C.State.get c rstate in
          let key =
            match Inline.Link.referenced_label l with
            | Some lbl -> Label.key lbl
            | None -> "^1"
          in
          let entry = register_footnote_ref st key fn in
          let fn_id = "user-content-fn-" ^ entry.slug in
          let ref_id =
            if entry.ref_count = 1 then "user-content-fnref-" ^ entry.slug
            else
              "user-content-fnref-" ^ entry.slug ^ "-"
              ^ Int.to_string entry.ref_count
          in
          C.string c "<sup";
          if entry.plain <> "" then begin
            C.string c " data-footnote-content=\"";
            C.string c (escape_html_text entry.plain);
            C.byte c '"'
          end;
          C.string c "><a href=\"#";
          C.string c (escape_html_text fn_id);
          C.string c "\" id=\"";
          C.string c (escape_html_text ref_id);
          C.string c "\" data-footnote-ref aria-describedby=\"footnote-label\">";
          C.string c (Int.to_string entry.index);
          C.string c "</a></sup>"
      | _ -> C.inline c (Inline.Link.text l)
      end;
      true
  | Inline.Image (i, _) ->
      begin match Inline.Link.reference_definition (C.get_defs c) i with
      | Some (Link_definition.Def (ld, _)) ->
          let raw_dest, title = link_dest_and_title ld in
          let encoded_dest = pct_encode_url raw_dest in
          let alt = inline_to_plain_string (Inline.Link.text i) in
          let is_safe =
            raw_dest <> ""
            && Safe_url.is_safe_url ~allow_contact:false ~decode_entities:false
                 encoded_dest
          in
          C.string c "<img loading=\"lazy\" decoding=\"async\"";
          begin match String.split_on_char 'x' title with
          | [w; h] ->
              begin match int_of_string_opt w, int_of_string_opt h with
              | Some width, Some height when width > 0 && height > 0 ->
                  C.string c (Printf.sprintf " width=\"%d\" height=\"%d\"" width height)
              | _ -> ()
              end
          | _ -> ()
          end;
          if is_safe then begin
            C.string c " src=\"";
            C.string c encoded_dest;
            C.byte c '"'
          end;
          C.string c " alt=\"";
          C.string c (escape_html_text alt);
          C.byte c '"';
          if title <> "" then begin
            C.string c " title=\"";
            C.string c (escape_html_text title);
            C.byte c '"'
          end;
          C.byte c '>'
      | _ -> ()
      end;
      true
  | Inline.Ext_math_span (ms, _) ->
      let tex =
        Inline.Math_span.tex_layout ms
        |> List.map Block_line.tight_to_string
        |> String.concat " "
      in
      C.string c "<code class=\"math-inline\">";
      C.string c (escape_html_text tex);
      C.string c "</code>";
      true
  | _ -> true

let render_list_item ~tight c (i, _) =
  let open_li () = C.string c "<li>" in
  let close_li () = C.string c "</li>\n" in
  let render_item_blocks blk =
    match blk with
    | Block.Blank_line _ -> ()
    | Block.Paragraph (p, _) when tight -> C.inline c (Block.Paragraph.inline p)
    | Block.Blocks (bs, _) ->
        let rec loop add_nl = function
          | [] -> ()
          | Block.Blank_line _ :: rest -> loop add_nl rest
          | Block.Paragraph (p, _) :: rest when tight ->
              C.inline c (Block.Paragraph.inline p);
              loop true rest
          | b :: rest ->
              if add_nl then C.byte c '\n';
              C.block c b;
              loop false rest
        in
        loop true bs
    | other ->
        C.byte c '\n';
        C.block c other
  in
  open_li ();
  begin match Block.List_item.ext_task_marker i with
  | None -> render_item_blocks (Block.List_item.block i)
  | Some (mark, _) ->
      begin match Block.List_item.task_status_of_task_marker mark with
      | `Unchecked -> C.string c "<input type=\"checkbox\" disabled> "
      | `Checked | `Other _ ->
          C.string c "<input type=\"checkbox\" checked disabled> "
      | `Cancelled -> C.string c "<input type=\"checkbox\" disabled> "
      end;
      render_item_blocks (Block.List_item.block i)
  end;
  close_li ()

let render_table c (t : Block.Table.t) =
  let align_attr = function
    | None -> ""
    | Some `Left -> " align=\"left\""
    | Some `Center -> " align=\"center\""
    | Some `Right -> " align=\"right\""
  in
  let rec render_cells tag aligns cols =
    match (aligns, cols) with
    | (a, _) :: a_rest, (col, _) :: c_rest ->
        C.byte c '<';
        C.string c tag;
        C.string c (align_attr (fst a));
        C.byte c '>';
        C.inline c col;
        C.string c "</";
        C.string c tag;
        C.string c ">\n";
        render_cells tag a_rest c_rest
    | [], (col, _) :: c_rest ->
        C.byte c '<';
        C.string c tag;
        C.byte c '>';
        C.inline c col;
        C.string c "</";
        C.string c tag;
        C.string c ">\n";
        render_cells tag [] c_rest
    | _ -> ()
  in
  let render_row tag aligns cols =
    C.string c "<tr>\n";
    render_cells tag aligns cols;
    C.string c "</tr>\n"
  in
  C.string c "<div class=\"table-wrapper\"><table>\n";
  let rec loop aligns in_tbody = function
    | [] -> if in_tbody then C.string c "</tbody>\n"
    | ((`Header cols, _), _) :: (((`Sep sep_aligns, _), _) :: rest) ->
        if in_tbody then C.string c "</tbody>\n";
        C.string c "<thead>\n";
        render_row "th" sep_aligns cols;
        C.string c "</thead>\n";
        loop sep_aligns false rest
    | ((`Header cols, _), _) :: rest ->
        if in_tbody then C.string c "</tbody>\n";
        C.string c "<thead>\n";
        render_row "th" aligns cols;
        C.string c "</thead>\n";
        loop aligns false rest
    | ((`Sep sep_aligns, _), _) :: rest -> loop sep_aligns in_tbody rest
    | ((`Data cols, _), _) :: rest ->
        if not in_tbody then C.string c "<tbody>\n";
        render_row "td" aligns cols;
        loop aligns true rest
  in
  loop [] false (Block.Table.rows t);
  C.string c "</table></div>\n"

let render_block c = function
  | Video_block { src; title } ->
      C.string c
        "<video class=\"article-video\" controls muted playsinline preload=\"metadata\"";
      begin match src with
      | Some valid_src ->
          C.string c " src=\"";
          C.string c (escape_html_text valid_src);
          C.byte c '"'
      | None -> ()
      end;
      begin match title with
      | Some t when t <> "" ->
          C.string c " aria-label=\"";
          C.string c (escape_html_text t);
          C.byte c '"'
      | _ -> ()
      end;
      C.string c "></video>\n";
      true
  | Callout_block { kind; expandable; expanded; title; body } ->
      let kind_str = Domain.callout_kind_to_string kind in
      C.string c (if expandable then "<details class=\"callout-" else "<blockquote class=\"callout-");
      C.string c kind_str;
      C.string c "\" data-callout=\"";
      C.string c kind_str;
      C.string c "\" data-expandable=\"";
      C.string c (if expandable then "true" else "false");
      C.string c "\" data-expanded=\"";
      C.string c (if expanded then "true" else "false");
      C.byte c '"';
      if expandable && expanded then C.string c " open";
      C.string c (if expandable then ">\n<summary class=\"callout-title\"><span class=\"callout-title-text\">"
        else ">\n<div class=\"callout-title\"><span class=\"callout-title-text\">");
      C.inline c title;
      C.string c (if expandable then "</span></summary>\n<div class=\"callout-content\">"
        else "</span></div>\n<div class=\"callout-content\">");
      List.iter (C.block c) body;
      C.string c (if expandable then "</div>\n</details>\n" else "</div>\n</blockquote>\n");
      true
  | Block.Paragraph (p, _) ->
      C.string c "<p>";
      C.inline c (Block.Paragraph.inline p);
      C.string c "</p>\n";
      true
  | Block.Heading (h, _) ->
      let st = C.State.get c rstate in
      let lvl = Block.Heading.level h in
      let lvl_str = Int.to_string lvl in
      let plain = String.trim (inline_to_plain_string (Block.Heading.inline h)) in
      let raw_id =
        match Block.Heading.id h with
        | Some (`Auto id | `Id id) -> id
        | None -> plain
      in
      let id = unique_heading_id st raw_id in
      if lvl = 2 || lvl = 3 then
        st.toc_rev <- { Domain.level = lvl; id; text = plain } :: st.toc_rev;
      C.string c "<h";
      C.string c lvl_str;
      C.string c " id=\"";
      C.string c (escape_html_text id);
      C.string c "\">";
      C.inline c (Block.Heading.inline h);
      C.string c "</h";
      C.string c lvl_str;
      C.string c ">\n";
      true
  | Block.Block_quote (bq, _) ->
      C.string c "<blockquote>\n";
      C.block c (Block.Block_quote.block bq);
      C.string c "</blockquote>\n";
      true
  | Block.Code_block (cb, _) ->
      let st = C.State.get c rstate in
      let info = Option.map fst (Block.Code_block.info_string cb) in
      let lang_opt =
        Option.map fst (Option.bind info Block.Code_block.language_of_info_string)
      in
      let lines = List.map fst (Block.Code_block.code cb) in
      begin match lang_opt with
      | Some lang when String.lowercase_ascii lang = "mermaid" || String.lowercase_ascii lang = "mmd" ->
          st.has_mermaid <- true;
          C.string c "<pre><code class=\"language-mermaid\">";
          List.iter
            (fun l ->
              C.string c (escape_code_text l);
              C.byte c '\n')
            lines;
          C.string c "</code></pre>\n"
      | Some lang when is_plain_text_lang lang ->
          C.string c "<pre><code class=\"language-";
          C.string c (escape_html_text lang);
          C.string c "\">";
          List.iter
            (fun l ->
              C.string c (escape_code_text l);
              C.byte c '\n')
            lines;
          C.string c "</code></pre>\n"
      | Some lang ->
          C.string c "<pre><code class=\"hljs language-";
          C.string c (escape_html_text lang);
          C.string c "\">";
          List.iter
            (fun l ->
              C.string c (highlight_code_line lang l);
              C.byte c '\n')
            lines;
          C.string c "</code></pre>\n"
      | None ->
          C.string c "<pre><code>";
          List.iter
            (fun l ->
              C.string c (escape_code_text l);
              C.byte c '\n')
            lines;
          C.string c "</code></pre>\n"
      end;
      true
  | Block.List (l, _) ->
      let tight = Block.List'.tight l in
      begin match Block.List'.type' l with
      | `Unordered _ ->
          C.string c "<ul>\n";
          List.iter (render_list_item ~tight c) (Block.List'.items l);
          C.string c "</ul>\n"
      | `Ordered (start, _) ->
          if start = 1 then C.string c "<ol>\n"
          else begin
            C.string c "<ol start=\"";
            C.string c (Int.to_string start);
            C.string c "\">\n"
          end;
          List.iter (render_list_item ~tight c) (Block.List'.items l);
          C.string c "</ol>\n"
      end;
      true
  | Block.Thematic_break _ ->
      C.string c "<hr>\n";
      true
  | Block.Ext_table (t, _) ->
      render_table c t;
      true
  | Block.Ext_math_block (cb, _) ->
      C.string c "<pre><code class=\"math-display\">";
      List.iter
        (fun l ->
          C.string c (escape_code_text (Block_line.to_string l));
          C.byte c '\n')
        (Block.Code_block.code cb);
      C.string c "</code></pre>\n";
      true
  | Block.Blocks (bs, _) ->
      List.iter (C.block c) bs;
      true
  | Block.Html_block _ | Block.Blank_line _
  | Block.Link_reference_definition _ | Block.Ext_footnote_definition _ ->
      true
  | _ -> true

let render_footnotes c (st : render_state) =
  if st.footnote_order <> [] then begin
    C.string c "<section data-footnotes class=\"footnotes\"><h2 id=\"footnote-label\" class=\"sr-only\">각주</h2><ol>\n";
    List.iter
      (fun key ->
        match String_map.find_opt key st.footnotes with
        | None -> ()
        | Some entry ->
            let fn_id = "user-content-fn-" ^ entry.slug in
            let ref_id = "user-content-fnref-" ^ entry.slug in
            C.string c "<li id=\"";
            C.string c (escape_html_text fn_id);
            C.string c "\">\n";
            C.block c (Block.Footnote.block entry.fn);
            C.string c "<a href=\"#";
            C.string c (escape_html_text ref_id);
            C.string c
              "\" data-footnote-backref class=\"data-footnote-backref\" aria-label=\"Back to content\">↩</a>\n</li>\n")
      st.footnote_order;
    C.string c "</ol></section>\n"
  end

let render_doc c d =
  C.block c (Doc.block d);
  let st = C.State.get c rstate in
  render_footnotes c st;
  true

let strip_trailing_newline (s : string) : string =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then String.sub s 0 (len - 1) else s

let normalize_headings base doc =
  let base = max 1 (min 6 base) in
  let rec blocks previous acc = function
    | [] -> (List.rev acc, previous)
    | b :: rest ->
        let b, previous = block previous b in
        blocks previous (b :: acc) rest
  and block previous = function
    | Block.Heading (h, meta) ->
        let level = min (previous + 1) (max base (Block.Heading.level h)) in
        (Block.Heading (Block.Heading.make ?id:(Block.Heading.id h)
           ~level (Block.Heading.inline h), meta), level)
    | Block.Blocks (bs, meta) ->
        let bs, previous = blocks previous [] bs in
        (Block.Blocks (bs, meta), previous)
    | Block.Block_quote (bq, meta) ->
        let b, previous = block previous (Block.Block_quote.block bq) in
        (Block.Block_quote (Block.Block_quote.make b, meta), previous)
    | Callout_block cb ->
        let body, previous = blocks previous [] cb.body in
        (Callout_block { cb with body }, previous)
    | Block.List (l, meta) ->
        let items, previous = List.fold_left (fun (acc, previous) (it, imeta) ->
          let b, previous = block previous (Block.List_item.block it) in
          let it = Block.List_item.make ?ext_task_marker:(Block.List_item.ext_task_marker it) b in
          ((it, imeta) :: acc, previous)) ([], previous) (Block.List'.items l) in
        (Block.List (Block.List'.make ~tight:(Block.List'.tight l)
          (Block.List'.type' l) (List.rev items), meta), previous)
    | other -> (other, previous)
  in
  let body, _ = block (base - 1) (Doc.block doc) in
  let defs = Label.Map.map (function
    | Block.Footnote.Def (fn, meta) ->
        let contents, _ = block (max 2 base - 1) (Block.Footnote.block fn) in
        Block.Footnote.Def (Block.Footnote.make ~defined_label:(Block.Footnote.defined_label fn)
          (Block.Footnote.label fn) contents, meta)
    | other -> other) (Doc.defs doc) in
  Doc.make ~nl:(Doc.nl doc) ~defs body

let render ?(heading_base = 1) (md : string) : rendered_markdown =
  let parsed = Doc.of_string ~heading_auto_ids:true ~strict:false md in
  let transformed = normalize_headings heading_base (transform_doc md parsed) in
  let captured_state : render_state option ref = ref None in
  let init_context c d =
    init_render_state c d;
    captured_state := Some (C.State.get c rstate)
  in
  let renderer =
    Cmarkit_renderer.make ~init_context ~inline:render_inline ~block:render_block
      ~doc:render_doc ()
  in
  let raw_html = Cmarkit_renderer.doc_to_string renderer transformed in
  let html = strip_trailing_newline raw_html in
  let plain_text = String.trim (block_to_plain_string (Doc.block transformed)) in
  match !captured_state with
  | None -> { html; toc = []; plain_text; has_mermaid = false }
  | Some st ->
      {
        html;
        toc = List.rev st.toc_rev;
        plain_text;
        has_mermaid = st.has_mermaid;
      }
