open Async_kernel

(** [set_default_max_redirects n] sets the maximum number of redirects we will do before giving up.
    [n] of 0 means redirects will not be followed.
    An exception will be thrown if [n] < 0.
    This default is only used if [?max_directs] is not passed to the functions below. *)
val set_default_max_redirects : int -> unit

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
