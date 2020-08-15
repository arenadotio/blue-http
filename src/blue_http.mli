open Async_kernel

val request_stream
  :  ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?headers:Cohttp.Header.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> Cohttp.Code.meth
  -> Uri.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val request
  :  ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?headers:Cohttp.Header.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> Cohttp.Code.meth
  -> Uri.t
  -> (Cohttp.Response.t * string) Deferred.t

val request_ignore_body
  :  ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?headers:Cohttp.Header.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> Cohttp.Code.meth
  -> Uri.t
  -> Cohttp.Response.t Deferred.t
