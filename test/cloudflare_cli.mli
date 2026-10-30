(** Run the real CLI against temporary workspaces using only OCaml's standard
    library and [Unix]. Preserve the filesystem and error-path regressions. *)
val run : executable:string -> tokens_path:string -> unit
