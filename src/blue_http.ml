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

let request ?interrupt ?headers ?chunked ?body meth uri =
  let%bind ssl_config =
    let hostname = Uri.host uri in
    default_ssl_config ?hostname ()
  in
  Cohttp_async.Client.call ?interrupt ?headers ?chunked ?body ~ssl_config meth uri
;;
