open Core
open Async
module Pool = Blue_http.For_testing.Pool

let () =
  Thread_safe.block_on_async_exn
  @@ fun () ->
  Alcotest_async.
    [ ( "wait < expire_timeout"
      , [ Time.Span.of_ms 10. ]
        |> List.map ~f:(fun wait ->
               let expire_timeout = Time.Span.(wait + of_ms 10.) in
               test_case
                 Time.Span.(
                   sprintf
                     "wait=%s, expire_timeout=%s"
                     (to_string_hum wait)
                     (to_string_hum expire_timeout))
                 `Quick
                 (fun () ->
                   let pool =
                     Pool.create
                       ~max_elements:1
                       ~expire_timeout
                       ~new_item:(fun () -> return @@ Uuid_unix.create ())
                       ~kill_item:(fun _ -> Deferred.unit)
                       ~check_item:(fun _ -> return true)
                       ()
                   in
                   let%bind uuid = Pool.enqueue pool (fun t -> return t) in
                   after wait
                   >>= fun () ->
                   Pool.enqueue pool (fun t -> return t)
                   >>| [%test_result: Uuid.t] ~expect:uuid)) )
    ; ( "wait > expire_timeout"
      , [ Time.Span.of_ms 10. ]
        |> List.map ~f:(fun expire_timeout ->
               let wait = Time.Span.(expire_timeout + of_ms 1000.) in
               test_case
                 Time.Span.(
                   sprintf
                     "wait=%s, expire_timeout=%s"
                     (to_string_hum wait)
                     (to_string_hum expire_timeout))
                 `Quick
                 (fun () ->
                   let pool =
                     Pool.create
                       ~max_elements:1
                       ~expire_timeout
                       ~new_item:(fun () -> return @@ Uuid_unix.create ())
                       ~kill_item:(fun _ -> Deferred.unit)
                       ~check_item:(fun _ -> return true)
                       ()
                   in
                   let%bind uuid = Pool.enqueue pool (fun t -> return t) in
                   after wait
                   >>= fun () ->
                   Pool.enqueue pool (fun t -> return t)
                   >>| [%test_result: Uuid.t]
                         ~equal:(fun a b -> Uuid.(a <> b))
                         ~expect:uuid)) )
    ]
  |> Alcotest_async.run "test_tls"
;;
