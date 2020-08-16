open Core_kernel

exception Unknown_uri_scheme of string [@@deriving sexp_of]

module T = struct
  type t =
    { scheme : [ `Http | `Https ]
    ; host : string
    ; port : int
    }
  [@@deriving compare, hash, sexp]
end

include T
module Table = Hashtbl.Make (T)

let to_uri { scheme; host; port } =
  let scheme =
    match scheme with
    | `Http -> "http"
    | `Https -> "https"
  in
  Uri.make ~scheme ~host ~port ()
;;

let to_string t = to_uri t |> Uri.to_string

let of_uri uri =
  let scheme =
    match Uri.scheme uri with
    | Some "https" -> `Https
    | Some "http" | None -> `Http
    | Some scheme -> raise (Unknown_uri_scheme scheme)
  in
  { scheme
  ; host = Uri.host_with_default ~default:"localhost" uri
  ; port =
      (match Uri.port uri, scheme with
      | Some port, _ -> port
      | None, `Http -> 80
      | None, `Https -> 443
      | _ -> assert false)
  }
;;
