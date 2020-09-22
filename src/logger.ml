open Core
open Async

let tag_key = Univ_map.Key.create ~name:"log_tags" [%sexp_of: (string * string) list]
let with_tags tags f = Scheduler.with_local tag_key (Some tags) ~f

let add_tags_if_present tags =
  let existing_tags = Option.value ~default:[] tags in
  match Scheduler.find_local tag_key with
  | None -> existing_tags
  | Some t -> t @ existing_tags
;;

let info ?tags =
  let tags = add_tags_if_present tags in
  Log.Global.info ~tags
;;

let debug ?tags =
  let tags = add_tags_if_present tags in
  Log.Global.debug ~tags
;;
