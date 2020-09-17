open Core_kernel
open Async_kernel

(* The HTTP 1.0 standard recommends 5 as the maximum number of redirects in a chain
   See section 10.3 of https://www.ietf.org/rfc/rfc2616.txt
   Chrome's max is 20 and curl's max is 50 *)
let default_max_redirects = ref 20

let set_default_max_redirects n =
  assert (n >= 0);
  default_max_redirects := n
;;

let with_redirects ?max_redirects uri f =
  let max_redirects = Option.value max_redirects ~default:!default_max_redirects in
  assert (max_redirects >= 0);
  let seen_uris = Hash_set.create (module String) in
  let rec loop ~max_redirects uri =
    Hash_set.add seen_uris (Uri.to_string uri);
    let%bind ((response, response_body) as res) = f uri in
    let status_code = Cohttp.(Response.status response |> Code.code_of_status) in
    if Cohttp.Code.is_redirection status_code
    then (
      match Cohttp.(Response.headers response |> Header.get_location) with
      | Some new_uri when Uri.to_string new_uri |> Hash_set.mem seen_uris ->
        Logger.debug
          "Ignoring %d redirect from %s to %s due to redirect loop detected"
          status_code
          (Uri.to_string uri)
          (Uri.to_string new_uri);
        return res
      | Some new_uri ->
        if max_redirects > 0
        then (
          Logger.debug
            "Following %d redirect from %s to %s"
            status_code
            (Uri.to_string uri)
            (Uri.to_string new_uri);
          (* Cohttp leaks connections if we don't drain the response body *)
          Cohttp_async.Body.drain response_body
          >>= fun () -> loop ~max_redirects:(max_redirects - 1) new_uri)
        else (
          Logger.debug
            "Ignoring %d redirect from %s to %s because we hit our redirect limit"
            status_code
            (Uri.to_string uri)
            (Uri.to_string new_uri);
          return res)
      | None ->
        Logger.debug
          "Ignoring %d redirect from %s because there is no Location header"
          status_code
          (Uri.to_string uri);
        return res)
    else return res
  in
  loop ~max_redirects uri
;;
