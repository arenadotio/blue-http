open Core_kernel
open Async_kernel
open Async_unix
open Timing

module Request = struct
  include Cohttp.Request

  include (
    Make (Cohttp_async.Io) : module type of Make (Cohttp_async.Io) with type t := t)
  end

module Response = struct
  include Cohttp.Response

  include (
    Make (Cohttp_async.Io) : module type of Make (Cohttp_async.Io) with type t := t)
  end

module Net = struct
  exception Failed_to_resolve_host of string [@@deriving sexp_of]

  let connect_uri ?interrupt { Scheme_host_port.scheme; host; port } =
    let open Async_unix.Unix in
    (* TODO: Cache DNS lookups until they expire *)
    let%bind addr =
      run_with_timing ~label:"time_dns_namelookup"
      @@ fun () ->
      Addr_info.get
        ~host
        [ Addr_info.AI_FAMILY PF_INET; Addr_info.AI_SOCKTYPE SOCK_STREAM ]
      >>| function
      | { Addr_info.ai_addr = ADDR_INET (addr, _); _ } :: _ ->
        Ipaddr_unix.of_inet_addr addr
      | _ -> raise (Failed_to_resolve_host host)
    in
    let%bind mode =
      run_with_timing ~label:"time_set_url_scheme"
      @@ fun () ->
      match scheme with
      | `Https ->
        let%map config = Ssl.default_ssl_config ~hostname:host () in
        `OpenSSL (addr, port, config)
      | `Http -> Deferred.return @@ `TCP (addr, port)
    in
    run_with_timing ~label:"time_tcp_connect"
    @@ fun () -> Conduit_async.V2.connect ?interrupt mode
  ;;
end

type t =
  { ic : Reader.t
  ; oc : Writer.t
  ; mutable connection_state : [ `New_connection | `Reused_connection ]
  }

let close { ic; oc; _ } =
  Deferred.both (Writer.close oc) (Reader.close ic) >>| fun ((), ()) -> ()
;;

let connect ?interrupt scheme_host_port =
  Net.connect_uri ?interrupt scheme_host_port
  >>| fun (ic, oc) ->
  let t = { ic; oc; connection_state = `New_connection } in
  Deferred.any [ Writer.consumer_left oc; Reader.close_finished ic ]
  >>= (fun () -> close t)
  |> don't_wait_for;
  t
;;

let is_closed { ic; oc; _ } = Reader.is_closed ic || Writer.is_closed oc

exception Connection_closed_by_remote_host of [ `New_connection | `Reused_connection ]
[@@deriving sexp_of]

let request ~body ({ ic; oc; _ } as t) req =
  Monitor.protect
    (fun () ->
      Request.write (Cohttp_async.Body_raw.write_body Request.write_body body) req oc
      >>= fun () ->
      Response.read ic
      >>| function
      | `Eof -> raise (Connection_closed_by_remote_host t.connection_state)
      | `Invalid reason -> failwith reason
      | `Ok resp ->
        let body =
          match Response.has_body resp with
          | `Yes | `Unknown ->
            `Pipe
              (Response.make_body_reader resp ic
              |> Cohttp_async.Body_raw.pipe_of_body Response.read_body_chunk)
          | `No -> `Empty
        in
        resp, body)
    ~finally:(fun () ->
      t.connection_state <- `Reused_connection;
      Deferred.unit)
;;

let call ?headers ?(chunked = false) ?(body = `Empty) t meth uri =
  let%bind req =
    match chunked with
    | false ->
      let%map _body, body_length = Cohttp_async.Body_raw.disable_chunked_encoding body in
      Request.make_for_client ?headers ~chunked ~body_length meth uri
    | true ->
      if%map Cohttp_async.Body.is_empty body
      then
        (* Don't used chunked encoding with an empty body *)
        Request.make_for_client ?headers ~chunked:false ~body_length:0L meth uri
      else
        (* Use chunked encoding if there is a body *)
        Request.make_for_client ?headers ~chunked:true meth uri
  in
  request ~body t req
;;
