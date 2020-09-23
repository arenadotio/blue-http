open Async_kernel
module Client = Client

module For_testing : sig
  module Pool = Pool
end

type tags = (string * string) list

(** [set_default_max_redirects n] sets the maximum number of redirects we will do before giving up.
    [n] of 0 means redirects will not be followed.
    An exception will be thrown if [n] < 0.
    This default is only used if [?max_directs] is not passed to the functions below. *)
val set_default_max_redirects : int -> unit

val call_stream
  :  ?tags:tags
  -> ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?headers:Cohttp.Header.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> ?client:Client.t
  -> Cohttp.Code.meth
  -> Uri.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val call
  :  ?tags:tags
  -> ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?headers:Cohttp.Header.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> ?client:Client.t
  -> Cohttp.Code.meth
  -> Uri.t
  -> (Cohttp.Response.t * string) Deferred.t

val call_ignore_body
  :  ?tags:tags
  -> ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?headers:Cohttp.Header.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> ?client:Client.t
  -> Cohttp.Code.meth
  -> Uri.t
  -> Cohttp.Response.t Deferred.t

val request_stream
  :  ?tags:tags
  -> ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> ?uri:Uri.t
  -> ?client:Client.t
  -> Cohttp.Request.t
  -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t

val request
  :  ?tags:tags
  -> ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> ?uri:Uri.t
  -> ?client:Client.t
  -> Cohttp.Request.t
  -> (Cohttp.Response.t * string) Deferred.t

val request_ignore_body
  :  ?tags:tags
  -> ?max_redirects:int
  -> ?interrupt:unit Deferred.t
  -> ?chunked:bool
  -> ?body:Cohttp_async.Body.t
  -> ?uri:Uri.t
  -> ?client:Client.t
  -> Cohttp.Request.t
  -> Cohttp.Response.t Deferred.t
