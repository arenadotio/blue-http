open Core
open Async
module Client = Client

let set_default_max_redirects = Redirect.set_default_max_redirects

let request_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri =
  let with_client f =
    match client with
    | Some client -> f client
    | None ->
      let client = Client.create () in
      Monitor.protect
        (fun () -> f client)
        ~finally:(fun () -> Client.close client |> Deferred.return)
  in
  with_client
  @@ fun client ->
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
