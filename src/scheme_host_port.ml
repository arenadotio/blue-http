open Core_kernel

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

let to_uri { scheme; host; port } = Uri.make ~scheme ~host ~port ()
let to_string t = to_uri t |> Uri.to_string

let of_uri uri =
  { scheme = Uri.scheme uri |> Option.value ~default:"http"
  ; host = Uri.host_with_default ~default:"localhost" uri
  ; port = Uri.port uri |> Option.value ~default:80
  }
;;
