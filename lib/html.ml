type attr = string * string

type node =
  | El of string * attr list * node list
  | Void of string * attr list
  | Txt of string
  | Raw of string
  | Frag of node list
  | Empty

let escape_text (s : string) : string =
  let len = String.length s in
  let buf = Buffer.create (len + 16) in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else begin
      begin match s.[i] with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '\x00' -> Buffer.add_utf_8_uchar buf Uchar.rep
      | c -> Buffer.add_char buf c
      end;
      loop (i + 1)
    end
  in
  loop 0

let escape_attr (s : string) : string =
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

let el tag ?(attrs = []) children = El (tag, attrs, children)
let void tag ?(attrs = []) () = Void (tag, attrs)
let txt s = Txt s
let raw s = Raw s
let frag nodes = Frag nodes
let empty = Empty

let write_attrs (buf : Buffer.t) (attrs : attr list) : unit =
  List.iter
    (fun (k, v) ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf k;
      if v <> "" || k = "value" || k = "alt" || k = "href" || k = "src" then begin
        Buffer.add_string buf "=\"";
        Buffer.add_string buf (escape_attr v);
        Buffer.add_char buf '"'
      end)
    attrs

let rec write_node (buf : Buffer.t) (n : node) : unit =
  match n with
  | Empty -> ()
  | Txt s -> Buffer.add_string buf (escape_text s)
  | Raw s -> Buffer.add_string buf s
  | Frag children -> List.iter (write_node buf) children
  | Void (tag, attrs) ->
      Buffer.add_char buf '<';
      Buffer.add_string buf tag;
      write_attrs buf attrs;
      Buffer.add_char buf '>'
  | El (tag, attrs, children) ->
      Buffer.add_char buf '<';
      Buffer.add_string buf tag;
      write_attrs buf attrs;
      Buffer.add_char buf '>';
      List.iter (write_node buf) children;
      Buffer.add_string buf "</";
      Buffer.add_string buf tag;
      Buffer.add_char buf '>'

let to_string (n : node) : string =
  let buf = Buffer.create 1024 in
  write_node buf n;
  Buffer.contents buf

let to_document ~(lang : string) ~(head : node list) ~(body : node list) :
    string =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "<!DOCTYPE html>\n";
  write_node buf
    (El
       ( "html",
         [ ("lang", lang) ],
         [ El ("head", [], head); El ("body", [], body) ] ));
  Buffer.add_char buf '\n';
  Buffer.contents buf
