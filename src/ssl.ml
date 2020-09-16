open Core_kernel
open Async_kernel
open Async_unix

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
    Logger.debug "Rejecting SSL connection with no server certificate";
    false
  | Some (Error e) ->
    Logger.debug
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
      Logger.debug "Rejecting certificate with no 'Common Name'";
      false
    | Some common_name ->
      let names = common_name :: Async_ssl.Ssl.Certificate.subject_alt_names cert in
      List.exists names ~f:(fun glob -> host_glob_matches ~glob hostname)
      |> (function
      | false ->
        Logger.debug
          !"Rejecting SSL connection to %s since none of the cert's host names match \
            (certificates host are: %{sexp:string list})"
          hostname
          names;
        false
      | true -> true))
;;

let default_ssl_config ~hostname () =
  let%map ca_file = Lazy_deferred.force_exn default_ca_file in
  let verify conn =
    [ Option.map ca_file ~f:(fun _ -> Conduit_async.Ssl.verify_certificate)
    ; Some (fun conn -> verify_certificate_host ~hostname conn |> Deferred.return)
    ]
    |> List.filter_opt
    |> Deferred.List.for_all ~f:(fun test -> test conn)
  in
  Conduit_async.V2.Ssl.Config.create ?ca_file ~verify ~hostname ()
;;
