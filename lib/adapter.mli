(** Pure deployment adapters. Static builds retain their existing layout;
    Cloudflare builds target assets-only Workers Build Output v0, as consumed
    by [cf deploy --prebuilt] (tested against cf 1.0.0-beta.25). *)
type t = Static | Cloudflare

val of_string : string -> (t, Domain.build_error) result
(** Parse [static] or [cloudflare]; reject unsupported adapters. *)

val cloudflare_output_dir : string
(** The generated Build Output directory, relative to the output root. *)

val with_static_files :
  generated:Site_builder.output_artifact list ->
  Site_builder.output_artifact list ->
  (Site_builder.output_artifact list, Domain.build_error) result
(** Append author-supplied static files to the generated artifacts, bytes
    unchanged. Static files never replace generated output: a static path that
    is unsafe (empty, absolute, containing [.] or [..] segments, backslashes or
    control characters), repeats another static path, equals a generated path,
    or would be both a file and a directory in the merged tree is rejected. *)

val adapt :
  t ->
  name:string ->
  compatibility_date:string ->
  Site_builder.output_artifact list ->
  (Site_builder.output_artifact list, Domain.build_error) result
(** Validate Cloudflare settings and artifact paths, then package only the
    supplied public artifacts. Use extensionless URLs and the generated 404
    page. No Worker code, credentials, filesystem access or clock is needed.
    Static output ignores the Cloudflare settings and is returned unchanged. *)
