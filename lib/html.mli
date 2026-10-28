(** Pure Functional HTML Algebraic Data Type (ADT) and Serializer.
    Guarantees automatic HTML escaping of all text and attribute values. *)

type attr = string * string

type node =
  | El of string * attr list * node list
  | Void of string * attr list
  | Txt of string
  | Raw of string
  | Frag of node list
  | Empty

val escape_text : string -> string
val escape_attr : string -> string

val el : string -> ?attrs:attr list -> node list -> node
val void : string -> ?attrs:attr list -> unit -> node
val txt : string -> node
val raw : string -> node
val frag : node list -> node
val empty : node

val to_string : node -> string
val to_document : lang:string -> head:node list -> body:node list -> string
