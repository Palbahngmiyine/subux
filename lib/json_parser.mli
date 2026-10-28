(** Pure functional RFC 8259 JSON parser and serializer with zero dependencies. *)

type value =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of value list
  | Object of (string * value) list

val parse : string -> (value, Domain.build_error) result
val to_string : value -> string
