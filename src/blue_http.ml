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

let default_ssl_config ?ca_file ?hostname () =
  let%map ca_file =
    match ca_file with
    | Some ca_file -> return (Some ca_file)
    | None -> Lazy_deferred.force_exn default_ca_file
  in
  let verify =
    match ca_file with
    | Some _ -> Some Conduit_async.Ssl.verify_certificate
    | None -> None
  in
  Conduit_async.V2.Ssl.Config.create ?ca_file ?verify ?hostname ()
;;

(* The HTTP 1.0 standard recommends 5 as the maximum number of redirects in a chain
   See section 10.3 of https://www.ietf.org/rfc/rfc2616.txt
   Chrome's max is 20 and curl's max is 50 *)
let default_max_redirects = 20

let rec with_redirects ?(max_redirects = default_max_redirects) uri f =
  let%bind ((response, response_body) as res) = f uri in
  let status_code = Cohttp.(Response.status response |> Code.code_of_status) in
  if Cohttp.Code.is_redirection status_code
  then (
    match Cohttp.(Response.headers response |> Header.get_location) with
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
        >>= fun () -> with_redirects ~max_redirects:(max_redirects - 1) new_uri f)
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
;;

let request ?max_redirects ?interrupt ?headers ?chunked ?body meth uri =
  with_redirects ?max_redirects uri
  @@ fun uri ->
  let%bind ssl_config =
    let hostname = Uri.host uri in
    default_ssl_config ?hostname ()
  in
  Cohttp_async.Client.call ?interrupt ?headers ?chunked ?body ~ssl_config meth uri
;;
