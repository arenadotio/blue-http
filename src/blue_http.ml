open Core
open Async

let set_default_max_redirects = Redirect.set_default_max_redirects

let request_stream ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  Redirect.with_redirects ?max_redirects uri
  @@ fun uri ->
  let%bind ssl_config = Ssl.default_ssl_config () in
  Cohttp_async.Client.call ?interrupt ~ssl_config ?headers ?chunked ?body meth uri
;;

let request ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?headers ?chunked ?body meth uri
  in
  Cohttp_async.Body.to_string body >>| fun body -> res, body
;;

let request_ignore_body ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?headers ?chunked ?body meth uri
  in
  Cohttp_async.Body.drain body >>| fun () -> res
;;
