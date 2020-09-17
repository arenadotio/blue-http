open Core
open Async

let key = Univ_map.Key.create ~name:"logging_uuid" Uuid.sexp_of_t
let with_uuid uuid f = Scheduler.with_local key (Some uuid) ~f

let with_uuid_tags tags =
  let open Option.Let_syntax in
  let tag =
    let%map uuid = Scheduler.find_local key in
    "request_uuid", Uuid.to_string uuid
  in
  Option.(to_list tag @ value ~default:[] tags)
;;

let info ?tags =
  let tags = with_uuid_tags tags in
  Log.Global.info ~tags
;;

let debug ?tags =
  let tags = with_uuid_tags tags in
  Log.Global.debug ~tags
;;
