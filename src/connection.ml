open Core_kernel
open Async_kernel
open Async_unix

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
  exception Unknown_uri_scheme of string [@@deriving sexp_of]

  let connect_uri ?interrupt { Scheme_host_port.scheme; host; port } =
    let open Async_unix.Unix in
    (* TODO: Cache DNS lookups until they expire *)
    let%bind addr =
      Addr_info.get
        ~host
        [ Addr_info.AI_FAMILY PF_INET; Addr_info.AI_SOCKTYPE SOCK_STREAM ]
      >>| function
      | { Addr_info.ai_addr = ADDR_INET (addr, _); _ } :: _ ->
        Ipaddr_unix.of_inet_addr addr
      | _ -> raise (Failed_to_resolve_host host)
    in
    let%bind mode =
      match scheme with
      | "https" ->
        let%map config = Ssl.default_ssl_config ~hostname:host () in
        `OpenSSL (addr, port, config)
      | "http" -> Deferred.return @@ `TCP (addr, port)
      | scheme -> raise (Unknown_uri_scheme scheme)
    in
    Conduit_async.V2.connect ?interrupt mode
  ;;
end

type t' =
  { ic : Reader.t
  ; oc : Writer.t
  }

(* we can't send concurrent requests over HTTP/1 *)
type t = t' Sequencer.t

let connect ?interrupt scheme_host_port =
  Net.connect_uri ?interrupt scheme_host_port
  >>| fun (ic, oc) ->
  let t = { ic; oc } |> Sequencer.create ~continue_on_error:false in
  Throttle.at_kill t (fun { ic; oc } ->
      Deferred.both (Writer.close oc) (Reader.close ic) >>| fun ((), ()) -> ());
  Deferred.any [ Writer.consumer_left oc; Reader.close_finished ic ]
  >>| (fun () -> Throttle.kill t)
  |> don't_wait_for;
  t
;;

let close t =
  Throttle.kill t;
  Throttle.cleaned t
;;

let is_closed t = Throttle.is_dead t

exception Connection_closed_by_remote_host [@@deriving sexp_of]

let request ~body t req =
  let res = Ivar.create () in
  Throttle.enqueue t (fun { ic; oc } ->
      Request.write (Cohttp_async.Body_raw.write_body Request.write_body body) req oc
      >>= fun () ->
      Response.read ic
      >>= function
      | `Eof -> raise Connection_closed_by_remote_host
      | `Invalid reason -> failwith reason
      | `Ok resp ->
        let body =
          match Response.has_body resp with
          | `Yes | `Unknown ->
            Response.make_body_reader resp ic
            |> Cohttp_async.Body_raw.pipe_of_body Response.read_body_chunk
          | `No -> Pipe.empty ()
        in
        Ivar.fill res (resp, `Pipe body);
        (* block starting any more requests until the consumer has finished reading this request *)
        Pipe.closed body)
  |> don't_wait_for;
  Ivar.read res
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
