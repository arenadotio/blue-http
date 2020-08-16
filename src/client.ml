open Core_kernel
open Async_kernel
open Async_unix

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
    ~max_elements:max_connections_per_host
    ~expire_timeout:connection_expire_timeout
    ~new_item:(fun () ->
      Log.Global.info
        "Making new connection to %s"
        (Scheme_host_port.to_string scheme_host_port);
      Connection.connect ?interrupt scheme_host_port)
    ~kill_item:(fun t ->
      Log.Global.info
        "Closing connection to %s"
        (Scheme_host_port.to_string scheme_host_port);
      Connection.close t)
    ~check_item:(fun t -> Deferred.return @@ not @@ Connection.is_closed t)
    ~on_empty:(fun () ->
      if Hashtbl.mem connections scheme_host_port
      then (
        Log.Global.info
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
  let rec loop ~is_first_request =
    Monitor.try_with (fun () ->
        let pool = find_or_make_pool ?interrupt t uri in
        let ivar_res = Ivar.create () in
        Pool.enqueue pool (fun connection ->
            let%bind res =
              Monitor.try_with ~extract_exn:true (fun () ->
                  Connection.call ?headers ?chunked ?body connection meth uri)
            in
            Ivar.fill ivar_res res;
            match res with
            | Ok (_, `Pipe body) ->
              Log.Global.info
                "Waiting for body to finish reading for %s"
                (Uri.to_string uri);
              (* We need to wait for the body to finish being read before we can re-use this connection *)
              Pipe.closed body
              >>| fun () ->
              Log.Global.info "Finished reading body for %s" (Uri.to_string uri)
            | _ -> Deferred.unit)
        |> don't_wait_for;
        Ivar.read ivar_res >>| Result.ok_exn)
    >>= function
    | Ok response ->
      Log.Global.info "Finished request to %s" (Uri.to_string uri);
      return response
    | Error e ->
      (* We have no way of detecting if the remote side closed a connection before we were able to make a request,
         so always make a request and retry once if the connection was closed *)
      let remote_connection_closed =
        match Monitor.extract_exn e with
        (* This exception is thrown by Cohttp if the other end hangs up *)
        | Connection.Connection_closed_by_remote_host when is_first_request ->
          true
          (* This exception is thrown if multiple requests are queued when the connection is closed *)
        | e when Exn.to_string e |> String.is_substring ~substring:"throttle aborted job"
          -> true
        | _ -> false
      in
      if remote_connection_closed
      then (
        Log.Global.debug
          "Remote connection to %s was closed, opening a new one"
          (Uri.to_string uri);
        loop ~is_first_request:false)
      else raise e
  in
  loop ~is_first_request:true
;;
