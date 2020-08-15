open Async

let () =
  let command =
    let open Command.Let_syntax in
    Command.async
      ~summary:"a more complicated example"
      [%map_open
        let meth =
          flag
            ~doc:"METHOD the HTTP method to use (default: GET)"
            "--method"
            ~aliases:[ "-m" ]
            (optional_with_default `GET (Arg_type.create Cohttp.Code.method_of_string))
        and uri = anon ("uri" %: Arg_type.create Uri.of_string) in
        fun () ->
          let open Deferred.Let_syntax in
          Log.Global.info "%s %s" (Cohttp.Code.string_of_method meth) (Uri.to_string uri);
          let%bind response, body = Blue_http.request meth uri in
          Log.Global.info !"%{sexp:Cohttp.Response.t}" response;
          let%map body = Cohttp_async.Body.to_string body in
          Log.Global.info "%s" body]
  in
  Command.run command
;;
