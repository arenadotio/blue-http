open Core_kernel
open Async_kernel

type t =
  { connections : Connection.t Pool.t Scheme_host_port.Table.t
  ; max_connections_per_host : int
  ; connection_expire_timeout : Time.Span.t
  }

(* Most browsers seem to support 6 concurrent connections
   https://stackoverflow.com/a/985704/212555 *)
let create
    ?(max_connections_per_host = 6)
    ?(connection_expire_timeout = Time.Span.of_int_sec 30)
    ()
  =
  { connections = Scheme_host_port.Table.create ()
  ; max_connections_per_host
  ; connection_expire_timeout
  }
;;

let close { connections; _ } =
  (* Pool.close deletes from connections from we need to iterate over a copy to avoid mutation during iteration *)
  Hashtbl.data connections
  |> Deferred.List.iter ~how:`Parallel ~f:(fun pool -> Pool.close pool)
;;

let make_pool
    ?interrupt
    { max_connections_per_host; connection_expire_timeout; connections; _ }
    scheme_host_port
  =
  Pool.create
    ~name:(Scheme_host_port.to_string scheme_host_port)
    ~max_elements:max_connections_per_host
    ~expire_timeout:connection_expire_timeout
    ~new_item:(fun () ->
      Logger.debug
        "Making new connection to %s"
        (Scheme_host_port.to_string scheme_host_port);
      Connection.connect ?interrupt scheme_host_port)
    ~kill_item:(fun t ->
      Logger.debug
        "Closing connection to %s"
        (Scheme_host_port.to_string scheme_host_port);
      Connection.close t)
    ~check_item:(fun t -> Deferred.return @@ not @@ Connection.is_closed t)
    ~on_empty:(fun () ->
      if Hashtbl.mem connections scheme_host_port
      then (
        Logger.debug
          "Last connection to %s closed"
          (Scheme_host_port.to_string scheme_host_port);
        Hashtbl.remove connections scheme_host_port))
    ()
;;

let find_or_make_pool ?interrupt t uri =
  let key = Scheme_host_port.of_uri uri in
  match Hashtbl.find t.connections key with
  | Some pool -> pool
  | None ->
    let pool = make_pool ?interrupt t key in
    Hashtbl.set t.connections ~key ~data:pool;
    pool
;;

let call ?interrupt ?headers ?chunked ?body (t : t) meth uri =
  Deferred.repeat_until_finished () (fun () ->
      Logger.debug "Attempt to call: %s" (Uri.to_string uri);
      let pool = find_or_make_pool ?interrupt t uri
      and ivar_res = Ivar.create () in
      Logger.debug
        ~tags:[ "pool_name", pool.name ]
        "Queue depth of request pool: %d" (Throttle.num_jobs_waiting_to_start pool.insert_lock);
      Pool.enqueue pool (fun connection ->
          let%bind res =
            Monitor.try_with ~extract_exn:true (fun () ->
                Connection.call ?headers ?chunked ?body connection meth uri)
          in
          Ivar.fill ivar_res res;
          match res with
          | Ok (_, `Pipe body) ->
            (* We need to wait for the body to finish being read before we can re-use this connection *)
            Pipe.closed body
          | _ -> Deferred.unit)
      |> don't_wait_for;
      Ivar.read ivar_res
      >>| function
      | Error e ->
        (* We have no way of detecting if the remote side closed a connection before we were able to make
           a request,so we just try it and retry if this was a re-used connection *)
        if match Monitor.extract_exn e with
           (* This exception is thrown by Connection if the other end hangs up *)
           | Connection.Connection_closed_by_remote_host `Reused_connection -> true
           | e ->
             (* This exception is thrown if multiple requests are queued when the connection is closed *)
             Exn.to_string e |> String.is_substring ~substring:"throttle aborted job"
        then (
          Logger.debug
            "Remote connection to %s was closed, opening a new one"
            (Uri.to_string uri);
          `Repeat ())
        else raise e
      | Ok res -> `Finished res)
;;
