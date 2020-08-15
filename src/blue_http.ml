open Core
open Async

let default_ca_file =
  (* List from https://golang.org/src/crypto/x509/root_linux.go *)
  let ca_bundle_paths =
    [ "/etc/ssl/certs/ca-certificates.crt" (* Debian/Ubuntu/Gentoo etc. *)
    ; "/etc/pki/tls/certs/ca-bundle.crt" (* Fedora/RHEL 6 *)
    ; "/etc/ssl/ca-bundle.pem" (* OpenSUSE *)
    ; "/etc/pki/tls/cacert.pem" (* OpenELEC *)
    ; "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem" (* CentOS/RHEL 7 *)
    ; "/etc/ssl/cert.pem" (* Alpine Linux *)
    ; "/usr/local/etc/openssl/cert.pem" (* OS X Homebrew OpenSSL *)
    ]
  in
  Lazy_deferred.create
  @@ fun () ->
  Deferred.List.find ca_bundle_paths ~f:(fun p ->
      match%map Sys.file_exists ~follow_symlinks:true p with
      | `Yes -> true
      | _ -> false)
;;

(* match a string to a glob where a * subdomain matches any subdomain
   i.e. the glob *.example.com matches anything.example.com
   but *x.example.com would only match the domain *x.example.com
   Note that globs only cover one level, so *.example.com does _not_ match
   a.b.example.com *)
let host_glob_matches ~glob hostname =
  let glob_parts = String.split ~on:'.' glob
  and host_parts = String.split ~on:'.' hostname in
  List.for_all2 glob_parts host_parts ~f:(fun glob_part host_part ->
      match glob_part with
      | "*" -> true
      | _ -> String.(glob_part = host_part))
  |> function
  | List.Or_unequal_lengths.Ok matches -> matches
  | Unequal_lengths -> false
;;

let verify_certificate_host ~hostname conn =
  match Async_ssl.Ssl.Connection.peer_certificate conn with
  | None ->
    Log.Global.debug "Rejecting SSL connection with no server certificate";
    false
  | Some (Error e) ->
    Log.Global.debug
      !"Rejecting SSL connection with invalid certificate: %s"
      (Error.to_string_hum e);
    false
  | Some (Ok cert) ->
    (match
       Async_ssl.Ssl.Certificate.subject cert
       |> List.find_map ~f:(function
              | "CN", name -> Some name
              | _ -> None)
     with
    | None ->
      Log.Global.debug "Rejecting certificate with no 'Common Name'";
      false
    | Some common_name ->
      let names = common_name :: Async_ssl.Ssl.Certificate.subject_alt_names cert in
      List.exists names ~f:(fun glob -> host_glob_matches ~glob hostname)
      |> (function
      | false ->
        Log.Global.debug
          !"Rejecting SSL connection to %s since none of the cert's host names match \
            (certificates host are: %{sexp:string list})"
          hostname
          names;
        false
      | true -> true))
;;

let default_ssl_config ?hostname () =
  let%map ca_file = Lazy_deferred.force_exn default_ca_file in
  let verify conn =
    [ Option.map ca_file ~f:(fun _ -> Conduit_async.Ssl.verify_certificate)
    ; Option.map hostname ~f:(fun hostname conn ->
          verify_certificate_host ~hostname conn |> Deferred.return)
    ]
    |> List.filter_opt
    |> Deferred.List.for_all ~f:(fun test -> test conn)
  in
  Conduit_async.V2.Ssl.Config.create ?ca_file ~verify ?hostname ()
;;

(* The HTTP 1.0 standard recommends 5 as the maximum number of redirects in a chain
   See section 10.3 of https://www.ietf.org/rfc/rfc2616.txt
   Chrome's max is 20 and curl's max is 50 *)
let default_max_redirects = ref 20

let set_default_max_redirects n =
  assert (n >= 0);
  default_max_redirects := n
;;

let with_redirects ?max_redirects uri f =
  let max_redirects = Option.value max_redirects ~default:!default_max_redirects in
  assert (max_redirects >= 0);
  let seen_uris = Hash_set.create (module String) in
  let rec loop ~max_redirects uri =
    Hash_set.add seen_uris (Uri.to_string uri);
    let%bind ((response, response_body) as res) = f uri in
    let status_code = Cohttp.(Response.status response |> Code.code_of_status) in
    if Cohttp.Code.is_redirection status_code
    then (
      match Cohttp.(Response.headers response |> Header.get_location) with
      | Some new_uri when Uri.to_string new_uri |> Hash_set.mem seen_uris ->
        Log.Global.debug
          "Ignoring %d redirect from %s to %s due to redirect loop detected"
          status_code
          (Uri.to_string uri)
          (Uri.to_string new_uri);
        return res
      | Some new_uri ->
        if max_redirects > 0
        then (
          Log.Global.debug
            "Following %d redirect from %s to %s"
            status_code
            (Uri.to_string uri)
            (Uri.to_string new_uri);
          (* Cohttp leaks connections if we don't drain the response body *)
          Cohttp_async.Body.drain response_body
          >>= fun () -> loop ~max_redirects:(max_redirects - 1) new_uri)
        else (
          Log.Global.debug
            "Ignoring %d redirect from %s to %s because we hit our redirect limit"
            status_code
            (Uri.to_string uri)
            (Uri.to_string new_uri);
          return res)
      | None ->
        Log.Global.debug
          "Ignoring %d redirect from %s because there is no Location header"
          status_code
          (Uri.to_string uri);
        return res)
    else return res
  in
  loop ~max_redirects uri
;;

let request_stream ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  with_redirects ?max_redirects uri
  @@ fun uri ->
  let%bind ssl_config =
    let hostname = Uri.host uri in
    default_ssl_config ?hostname ()
  in
  Cohttp_async.Client.call ?interrupt ?headers ?chunked ?body ~ssl_config meth uri
;;

let request ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?headers ?chunked ?body meth uri
  in
  Cohttp_async.Body.to_string body >>| fun body -> res, body
;;

let request_ignore_body ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  let%bind res, body =
    request_stream ?max_redirects ?interrupt ?headers ?chunked ?body meth uri
  in
  Cohttp_async.Body.drain body >>| fun () -> res
;;
