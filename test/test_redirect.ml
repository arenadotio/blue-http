open Core
open Async

let response_is_redirect res =
  Cohttp.Response.status res |> Cohttp.Code.code_of_status |> Cohttp.Code.is_redirection
;;

let () =
  Thread_safe.block_on_async_exn
  @@ fun () ->
  let%bind server =
    Cohttp_async.Server.create
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun ~body:_ _ req ->
        let uri = Cohttp.Request.uri req in
        Uri.get_query_param uri "n"
        |> Option.value_exn ~here:[%here]
        |> Int.of_string
        |> function
        | n when n > 0 ->
          let new_uri = Uri.with_query' uri [ "n", n - 1 |> Int.to_string ] in
          Cohttp_async.Server.respond_with_redirect new_uri
        | 0 -> Cohttp_async.Server.respond_string ~status:`OK "Not redirecting"
        | _ -> assert false)
  in
  let base_uri =
    let port = Cohttp_async.Server.listening_on server in
    Uri.make ~scheme:"http" ~host:"localhost" ~port ()
  in
  Alcotest_async.
    [ ( "redirects = max_redirects"
      , [ 0; 1; 2 ]
        |> List.map ~f:(fun redirects ->
               test_case (sprintf !"redirects=%d" redirects) `Quick (fun () ->
                   Blue_http.request_ignore_body
                     ~max_redirects:redirects
                     `GET
                     (Uri.with_query' base_uri [ "n", Int.to_string redirects ])
                   >>| [%test_pred: Cohttp.Response.t] (Fn.non response_is_redirect))) )
    ; ( "redirects < max_redirects"
      , [ 0; 1; 2 ]
        |> List.map ~f:(fun redirects ->
               let max_redirects = redirects + 1 in
               test_case
                 (sprintf !"redirects=%d, max_redirects:%d" redirects max_redirects)
                 `Quick
                 (fun () ->
                   Blue_http.request_ignore_body
                     ~max_redirects
                     `GET
                     (Uri.with_query' base_uri [ "n", Int.to_string redirects ])
                   >>| [%test_pred: Cohttp.Response.t] (Fn.non response_is_redirect))) )
    ; ( "redirects > max_redirects"
      , [ 0; 1; 2 ]
        |> List.map ~f:(fun max_redirects ->
               let redirects = max_redirects + 10 in
               test_case
                 (sprintf !"redirects=%d, max_redirects:%d" redirects max_redirects)
                 `Quick
                 (fun () ->
                   Blue_http.request_ignore_body
                     ~max_redirects
                     `GET
                     (Uri.with_query' base_uri [ "n", Int.to_string redirects ])
                   >>| [%test_pred: Cohttp.Response.t] response_is_redirect)) )
    ]
  |> Alcotest_async.run "test_redirect"
;;
