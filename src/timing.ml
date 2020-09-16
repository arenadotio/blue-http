open Core
open Async

let run_with_timing ?tags ~label f =
  let start = Time_ns.now () in
  f ()
  >>| fun res ->
  let stop = Time_ns.now () in
  Log.Global.info
    ?tags
    !"%s: %s "
    label
    (Time_ns.abs_diff start stop |> Time_ns.Span.to_string_hum);
  res
;;
