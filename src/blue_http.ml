open Core
open Async
module Client = Client

module For_testing = struct
  module Pool = Pool
end

let set_default_max_redirects = Redirect.set_default_max_redirects

let maybe_with_client ?client f =
  match client with
  | Some client -> f client
  | None ->
    let client = Client.create () in
    Monitor.protect
      (fun () -> f client)
      ~finally:(fun () ->
        (* We can't wait for the client to close because it doesn't finish until the body is fully read *)
        Client.close client |> don't_wait_for;
        Deferred.unit)
;;

let call_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri =
  maybe_with_client ?client
  @@ fun client ->
  Redirect.with_redirects ?max_redirects uri
  @@ fun uri -> Client.call ?interrupt ?headers ?chunked ?body client meth uri
;;

let request_stream ?max_redirects ?interrupt ?chunked ?body ?uri ?client req =
  let uri =
    match uri with
    | Some t -> t
    | None -> Cohttp.Request.uri req
  and headers = Cohttp.Request.headers req
  and meth = Cohttp.Request.meth req in
  Redirect.with_redirects ?max_redirects uri
  @@ fun uri -> call_stream ?interrupt ~headers ?chunked ?body ?client meth uri
;;

let call ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri =
  let%bind res, body =
    call_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri
  in
  Cohttp_async.Body.to_string body >>| fun body -> res, body
;;

let request ?max_redirects ?interrupt ?chunked ?body ?uri ?client req =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?chunked ?body ?uri ?client req
  in
  Cohttp_async.Body.to_string body >>| fun body -> res, body
;;

let call_ignore_body ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri =
  let%bind res, body =
    call_stream ?max_redirects ?interrupt ?headers ?chunked ?body ?client meth uri
  in
  Cohttp_async.Body.drain body >>| fun () -> res
;;

let request_ignore_body ?max_redirects ?interrupt ?chunked ?body ?uri ?client req =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?chunked ?body ?uri ?client req
  in
  Cohttp_async.Body.drain body >>| fun () -> res
;;
