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
                       ~name:"test"
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
                       ~name:"test"
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
      , [ test_case "check failed" `Quick (fun () ->
              let pool =
                let open T in
                Pool.create
                  ~name:"test"
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
        ; test_case "exception thrown" `Quick (fun () ->
              let pool =
                let open T in
                Pool.create
                  ~name:"test"
                  ~max_elements:1
                  ~expire_timeout:(Time.Span.of_day 100.)
                  ~new_item
                  ~kill_item
                  ~check_item
                  ()
              in
              let%bind t = Pool.enqueue pool Deferred.return in
              let%bind () =
                Deferred.Or_error.try_with (fun () ->
                    Pool.enqueue pool (fun _ -> raise Caml.Not_found))
                >>| [%test_pred: unit Or_error.t] Or_error.is_error
              in
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
                   ~name:"test"
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
                      after wait >>= fun () -> Pool.enqueue pool (fun _ -> Deferred.unit))
               >>= fun () ->
               Pool.enqueue pool Deferred.return >>| [%test_result: T.t] ~expect:t))
        ] )
    ; ( "race"
      , [ test_case "add 4 elements to 2 element pool" `Quick (fun () ->
              let pool =
                let open T in
                Pool.create
                  ~name:"test"
                  ~max_elements:2
                  ~expire_timeout:(Time.Span.of_ms 10.)
                  ~new_item
                  ~kill_item
                  ~check_item
                  ()
              in
              [ 1; 2; 3; 4 ]
              |> Deferred.List.iter ~how:`Parallel ~f:(fun _ ->
                     Pool.enqueue pool (fun _ -> Deferred.unit))
              >>| fun () -> [%test_result: int] (Pool.length pool) ~expect:2)
        ; test_case "kill item while other waiting" `Quick (fun () ->
              let pool =
                let open T in
                Pool.create
                  ~name:"test"
                  ~max_elements:1
                  ~expire_timeout:(Time.Span.of_day 100.)
                  ~new_item
                  ~kill_item
                  ~check_item
                  ()
              in
              let blocker = Ivar.create () in
              let%bind first_item = Pool.enqueue pool Deferred.return in
              Pool.enqueue pool (fun item ->
                  item.killed <- true;
                  Ivar.read blocker)
              |> don't_wait_for;
              let d =
                (* Enqueue two requests. The first one will detect that the item has been killed and will create
              another one, and the second one should detect that the pool size changed and will restart its wait *)
                [ (); () ]
                |> Deferred.List.map ~how:`Parallel ~f:(fun () ->
                       Pool.enqueue pool Deferred.return)
              in
              Scheduler.yield ()
              >>= fun () ->
              Ivar.fill blocker ();
              d
              >>| fun uuids ->
              (* The two requests should get the same UUID, and it should not be the first item, which was killed *)
              [%test_pred: T.t list] (List.contains_dup ~compare:T.compare) uuids;
              List.iter
                uuids
                ~f:([%test_result: T.t] ~equal:(fun a b -> T.(a <> b)) ~expect:first_item))
        ] )
    ]
  |> Alcotest_async.run "test_pool"
;;
