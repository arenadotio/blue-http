open Core
open Async
module Pool = Blue_http.For_testing.Pool

module T = struct
  type t =
    { uuid : Uuid.t
    ; mutable killed : bool
    }
  [@@deriving compare, equal, sexp_of]

  let ( = ) = equal
  let ( <> ) a b = not (a = b)
  let new_item () = Deferred.return { uuid = Uuid_unix.create (); killed = false }
  let kill_item t = Deferred.return @@ (t.killed <- true)
  let check_item { killed; _ } = Deferred.return @@ not killed
end

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
                     let open T in
                     Pool.create
                       ~max_elements:2
                       ~expire_timeout
                       ~new_item
                       ~kill_item
                       ~check_item
                       ()
                   in
                   let%bind t = Pool.enqueue pool Deferred.return in
                   after wait
                   >>= fun () ->
                   Pool.enqueue pool (fun t -> return t)
                   >>| [%test_result: T.t] ~expect:t
                   >>| fun () -> [%test_result: bool] t.killed ~expect:false)) )
    ; ( "wait > expire_timeout"
      , [ Time.Span.of_ms 10. ]
        |> List.map ~f:(fun expire_timeout ->
               let wait = Time.Span.(expire_timeout + of_ms 10.) in
               test_case
                 Time.Span.(
                   sprintf
                     "wait=%s, expire_timeout=%s"
                     (to_string_hum wait)
                     (to_string_hum expire_timeout))
                 `Quick
                 (fun () ->
                   let pool =
                     let open T in
                     Pool.create
                       ~max_elements:2
                       ~expire_timeout
                       ~new_item
                       ~kill_item
                       ~check_item
                       ()
                   in
                   let%bind t = Pool.enqueue pool Deferred.return in
                   after wait
                   >>= fun () ->
                   Pool.enqueue pool Deferred.return
                   >>| [%test_result: T.t] ~equal:(fun a b -> T.(a <> b)) ~expect:t
                   >>| fun () -> [%test_result: bool] t.killed ~expect:true)) )
    ; ( "killed"
      , [ test_case "killed" `Quick (fun () ->
              let pool =
                let open T in
                Pool.create
                  ~max_elements:1
                  ~expire_timeout:(Time.Span.of_day 100.)
                  ~new_item
                  ~kill_item
                  ~check_item
                  ()
              in
              let%bind t = Pool.enqueue pool Deferred.return in
              t.killed <- true;
              Pool.enqueue pool Deferred.return
              >>| [%test_result: T.t] ~equal:(fun a b -> T.(a <> b)) ~expect:t)
        ] )
    ; ( "wait > expire_timeout with re-use"
      , [ (let expire_timeout = Time.Span.of_ms 10.
           and wait = Time.Span.of_ms 5.
           and num_waits = 3 in
           test_case
             Time.Span.(
               sprintf
                 "wait=%s, expire_timeout=%s, num_waits=%d"
                 (to_string_hum wait)
                 (to_string_hum expire_timeout)
                 num_waits)
             `Quick
             (fun () ->
               let pool =
                 let open T in
                 Pool.create
                   ~max_elements:2
                   ~expire_timeout
                   ~new_item
                   ~kill_item
                   ~check_item
                   ()
               in
               let%bind t = Pool.enqueue pool Deferred.return in
               List.init num_waits ~f:(Fn.const ())
               |> Deferred.List.iter ~how:`Sequential ~f:(fun () ->
                      after wait
                      >>= fun () -> Pool.enqueue pool Deferred.return |> Deferred.ignore_m)
               >>= fun () ->
               Pool.enqueue pool Deferred.return >>| [%test_result: T.t] ~expect:t))
        ] )
    ; ( "race with max_elements"
      , [ test_case "add 4 elements to 2 element pool" `Quick (fun () ->
              let pool =
                let open T in
                Pool.create
                  ~max_elements:2
                  ~expire_timeout:(Time.Span.of_ms 10.)
                  ~new_item
                  ~kill_item
                  ~check_item
                  ()
              in
              [ 1; 2; 3; 4 ]
              |> Deferred.List.iter ~how:`Parallel ~f:(fun _ ->
                     Pool.enqueue pool Deferred.return |> Deferred.ignore_m)
              >>| fun () -> [%test_result: int] (Pool.length pool) ~expect:2)
        ] )
    ]
  |> Alcotest_async.run "test_tls"
;;
