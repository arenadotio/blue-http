open Core
open Async

let bad_tls_domains =
  [ "expired.badssl.com"
    (* FIXME: Verify host matches certificate
       ; "wrong.host.badssl.com" *)
  ; "self-signed.badssl.com"
  ; "untrusted-root.badssl.com"
  ]
;;

let () =
  Thread_safe.block_on_async_exn
  @@ fun () ->
  Alcotest_async.
    [ ( "invalid certificates"
      , bad_tls_domains
        |> List.map ~f:(fun domain ->
               test_case domain `Quick (fun () ->
                   Deferred.Or_error.try_with (fun () ->
                       Blue_http.request `GET (Uri.make ~scheme:"https" ~host:domain ()))
                   >>| [%test_pred: (Cohttp.Response.t * string) Or_error.t]
                         Or_error.is_error)) )
    ; ( "valid certificates"
      , [ test_case "example.com" `Quick (fun () ->
              Blue_http.request_ignore_body `GET (Uri.of_string "https://www.example.com")
              |> Deferred.ignore_m)
        ] )
    ]
  |> Alcotest_async.run "test_tls"
;;
