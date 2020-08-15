open Core
open Async
module Client = Client

let set_default_max_redirects = Redirect.set_default_max_redirects
let default_client = lazy (Client.create ())

let request_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri =
  let client =
    match client with
    | Some client -> client
    | None -> Lazy.force default_client
  in
  Redirect.with_redirects ?max_redirects uri
  @@ fun uri -> Client.call ?interrupt ?headers ?chunked ?body client meth uri
;;

let request ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri
  in
  Cohttp_async.Body.to_string body >>| fun body -> res, body
;;

let request_ignore_body ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri
  =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri
  in
  Cohttp_async.Body.drain body >>| fun () -> res
;;
