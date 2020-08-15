open Core_kernel
open Async_kernel
open Async_unix

module Scheme_host_port = struct
  module T = struct
    type t =
      { scheme : string
      ; host : string
      ; port : int
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  module Table = Hashtbl.Make (T)

  let of_uri uri =
    { scheme = Uri.scheme uri |> Option.value ~default:"http"
    ; host = Uri.host_with_default ~default:"localhost" uri
    ; port = Uri.port uri |> Option.value ~default:80
    }
  ;;
end

type t = Connection.t Scheme_host_port.Table.t

let create () = Scheme_host_port.Table.create ()

let call ?interrupt ?headers ?chunked ?body t meth uri =
  let rec loop ~is_first_request =
    Monitor.try_with (fun () ->
        let%bind connection =
          let key = Scheme_host_port.of_uri uri in
          match Hashtbl.find t key with
          | Some connection when not (Connection.is_closed connection) ->
            return connection
          | _ ->
            let%map connection = Connection.connect ?interrupt uri in
            Hashtbl.set t ~key ~data:connection;
            connection
        in
        Connection.call ?headers ?chunked ?body connection meth uri)
    >>= function
    | Ok response -> return response
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
