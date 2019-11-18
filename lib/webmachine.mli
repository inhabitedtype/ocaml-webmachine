(*----------------------------------------------------------------------------
    Copyright (c) 2015 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

(** A REST toolkit for OCaml.

    Webmachine is a port of the Erlang project of the
    {{:http://webmachine.github.io/}same name}, designed to work as a
    REST-aware layer on top of {{:https://github.com/mirage/ocaml-cohttp} cohttp}.

    To use this module, apply the {{!Make}[Make]} functor to an {{!IO}[IO]}
    and a {{!CLOCK}[CLOCK]} module, and subclass the
    {{!classtype:S.resource}resouce} virtual class. *)

open Cohttp

(** The [IO] module signature abstracts over monadic futures library. It is a
    much reduced version of the module signature that appears in Cohttp, and as
    such is compatible with any module that conforms to [Cohttp.S.IO]. *)
module type IO = sig
  type +'a t
  (** The type of a blocking computation *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** The monadic bind operator for the type ['a t]. [m >>= f] will pass the
      result of [m] to [f], once the result is determined. *)

  val return : 'a -> 'a t
  (** [return a] creates a value of type ['a t] that is already determined. *)
end

(** The [Rd] module is the means by which handlers access and manipulate
    request-specific information. *)
module Rd : sig
  type 'body t =
    { version       : Code.version
    ; meth          : Code.meth
    ; uri           : Uri.t
    ; req_headers   : Header.t
    ; req_body      : 'body
    ; resp_headers  : Header.t
    ; resp_body     : 'body
    ; resp_redirect : bool
    ; dispatch_path : string
    ; path_info     : (string * string) list
    } constraint 'body = [> `Empty]

  val make : ?dispatch_path:string -> ?path_info:(string * string) list
    -> ?resp_headers:Header.t -> ?resp_body:'a -> ?resp_redirect:bool
    -> ?req_body:'a -> request:Request.t
    -> unit -> 'a t
  (** [make ~request ()] returns a ['body t] with the following fields
      pouplated from the [request] argument:
      {ul
      {- [uri]};
      {- [version]};
      {- [meth]}; and
      {- [req_headers]}}.

      All other fields will be populated with default values unless they are
      provided as optional arguments *)

  val with_req_headers  : (Header.t -> Header.t) -> 'a t -> 'a t
  (** [with_req_headers f t] is equivalent to [{ t with req_headers = f (t.req_headers) }] *)

  val with_resp_headers : (Header.t -> Header.t) -> 'a t -> 'a t
  (** [with_resp_headers f t] is equivalent to [{ t with resp_headers = f (t.resp_headers) }] *)

  val lookup_path_info      : string -> 'a t -> string option
  val lookup_path_info_exn  : string -> 'a t -> string
  (** [lookup_path_info_exn k t] is equivalent [List.assoc k t.path_info],
      which will throw a [Not_found] exception if the lookup fails. The
      non-[_exn] version will return an optional result. *)

  val redirect : string -> 'a t -> 'a t
  (** [redirect location t] sets the [resp_redirect] bit in [t] and sets the
      [Location] response header to [location]. *)
end

module type S = sig
  type +'a io

  type 'a result =
    | Ok of 'a
    | Error of int

  type ('a, 'body) op = 'body Rd.t -> ('a result * 'body Rd.t) io
  type 'body provider = ('body, 'body) op
  type 'body acceptor = (bool, 'body) op

  type www_authenticate =
    { scheme : string; realm : string; params : (string * string) list }

  type auth =
    [ `Authorized
    | `Basic of string
    | `Challenge of www_authenticate
    | `Redirect of Uri.t
    ]

  val continue : 'a -> ('a, 'body) op
  (** [continue a rd] is equivalent to [IO.return (Ok x, rd)] *)

  val respond : ?body:'body -> int -> ('a, 'body) op
  (** [respond ?body n rd] is equivalent to [IO.return (Error n, { rd with resp_body = body }] *)

  (** The resource parent class. *)
  class virtual ['body] resource : object
    constraint 'body = [> `Empty]

    method virtual content_types_provided : ((string * ('body provider)) list, 'body) op
    method virtual content_types_accepted : ((string * ('body acceptor)) list, 'body) op

    method resource_exists : (bool, 'body) op
    (** Returning [false] will result in a [404 Not Found].

        {i Default}: [true] *)

    method service_available : (bool, 'body) op
    (** Returning [false] will result in a [503 Service Unavilable].

        {i Default}: [true] *)

    method is_authorized : (auth, 'body) op

    method forbidden : (bool, 'body) op
    (** Returning [true] will result in a [403 Forbidden].

        {i Default}: [false] *)

    method malformed_request : (bool, 'body) op
    (** Returning [true] will result in a [400 Bad Request].

        {i Default}: [false] *)

    method uri_too_long : (bool, 'body) op
    (** Returning [true] will result in a [414 Request-URI Too Long].

        {i Default}: [false] *)

    method known_content_type : (bool, 'body) op
    (** Returning [false] will result in a [415 Unsupported Media Type].

        {i Default}: [true] *)

    method valid_content_headers : (bool, 'body) op
    (** Returning [false] will result in a [501 Not Implemented].

        {i Default}: [true] *)

    method valid_entity_length : (bool, 'body) op
    (** Returning [false] will result in [413 Request Entity Too Large].

        {i Default} : [true] *)

    method options : ((string * string) list, 'body) op
    (** If the [`OPTIONS] method is supported by this resource, the returned
        list of header name/value pairs will be included in responses to
        [`OPTIONS] requests.

        {i Default}: [\[("allow", self#allowed_methods)\]] *)

    method known_methods : (Code.meth list, 'body) op
    (** A request to this resource whose method is not included in the returned
        list will result in a [501 Not Implemented].

        {i Default}: [\[`GET; `HEAD; `POST; `PUT; `DELETE; `Other "TRACE"; `Other "CONNECT"; `OPTIONS\]] *)

    method allowed_methods : (Code.meth list, 'body) op
    (** A request to this resource whose method is not included in the returned
        list will result in a [405 Method Not Allowed]. The response will
        include an ["allow"] header that lists the allowed methods.

        {i Default}: [\[`GET; `HEAD\]] *)

    method delete_resource : (bool, 'body) op
    (** [`DELETE] requests will call this method. Returning [true] indicates
        that the deletion succeeded.

        {i Default}: [false] *)

    method delete_completed : (bool, 'body) op
    (** Only successful {{!delete_resource}delete_resource} calls will result
        in a call to this method. Return [false] if the deletion was accepted
        but cannot yet be guaranteed to have finished.

        {i Default}: [true] *)

    method process_post : (bool, 'body) op
    (** [`POST] requests will call this method. Returning true indicates the
         POST succeeded. *)

    method language_available : (bool, 'body) op
    (** Returning [false] will result in a [406 Not Acceptable].

        {i Default}: [true] *)

    method charsets_provided : ((string * ('body -> 'body)) list, 'body) op
    method encodings_provided : ((string * ('body -> 'body)) list, 'body) op

    method variances : (string list, 'body) op
    (** Returns a list of header names that should be included in the
        response's [Vary] header. The standard content negotiation headers
        (i.e., [Accept], [Accept-Encoding], [Accept-Charset], [Accept-Language])
        will automatically be added if needed, and therefore do not need to be
        specified here.

        {i Default}: [[]] *)

    method is_conflict : (bool, 'body) op
    (** Returning [true] will result in a [409 Conflict].

        {i Default}: false *)

    method multiple_choices : (bool, 'body) op
    (** Returning [true] indicates that multiple representations of the
        resource exist and a single one cannot be automatically chosen, resulting
        in a [300 Multiple Choices] rather than a [200 OK].

        {i Default}: false *)

    method previously_existed : (bool, 'body) op
    method moved_permanently : (Uri.t option, 'body) op
    method moved_temporarily : (Uri.t option, 'body) op

    method last_modified : (string option, 'body) op
    method expires : (string option, 'body) op

    method generate_etag : (string option, 'body) op
    (** Returning [Some string] will use [string] for the [ETag] header.

        {i Default}: [None] *)

    method finish_request : (unit, 'body) op
    (** This method is called just before the final response is constructed and
        returned. *)

    method post_is_create : (bool, 'body) op
    (** If POST requests should be treated as a request to put content into a 
        (potentially new) resource as opposed to being a generic submission for 
        processing, then this function should return true. If it does return true, 
        then create_path will be called and the rest of the request will be treated 
        much like a PUT to the Path entry returned by that call. 

        {i Default}: false *)

    method create_path : (string, 'body) op
    (** This will be called on a POST request if post_is_create returns true. It is 
        an error for this function not to produce a Path if post_is_create returns 
        true. The Path returned should be a valid URI part following the dispatcher 
        prefix. That Path will replace the previous one in the return value of 
        wrq:disp_path(ReqData) for all subsequent resource function calls in the 
        course of this request. *)
  end

  val to_handler :
    ?dispatch_path:string -> ?path_info:(string * string) list ->
    resource:('body resource) -> body:'body -> request:Request.t -> unit ->
    (Code.status_code * Header.t * 'body * string list) io
  (** [to_handler ~resource ~body ~request ()] runs the resource through the
      HTTP decision diagram given [body] and [request]. The result is a tuple
      that contains the status code, headers and body of the response. The
      final element of the tuple is a list of decision diagram node names that
      is useful for debugging. *)

  val dispatch :
    ((Dispatch.tag * string) list * Dispatch.typ * (unit -> 'body resource)) list ->
    body:'body -> request:Request.t ->
    (Code.status_code * Header.t * 'body * string list) option io
  (** [dispatch routes] returns a request handler that will iterate through
      [routes] and dispatch the request to the first resources that matches the
      URI path. The form that the individal route entries takes this the
      following:

        {[(pattern, exact, resource_constructor)]}

      The [pattern] itself is a list of literal ([`Lit]) or variable matches
      ([`Var]) that the URI path should satify. For example, a route entry that
      will be associated with a particular user in the system would look like
      this:

        {[([`Lit, "user"; `Var, "id"], `Exact, user_resource)]}

      This would match a URI path such as ["/user/10"] but would not match a
      URI such as ["/usr/10/preferences"], since the [exact] component of the
      route tuple is [`Exact].
   *)

  val dispatch' :
    (string * (unit -> 'body resource)) list ->
    body:'body -> request:Request.t ->
    (Code.status_code * Header.t * 'body * string list) option io
  (** [dispatch' routes ~body ~request] works in the same way as {dispatch'}
      except the user can specify path patterns using a string shorthand. For
      example, the following route entry:

        {[("/user/:id/*", user_resource)]}

      translates to:

        {[([`Lit, "user"; `Var "id"], `Prefix, user_resource)]} *)
end

(** The [CLOCK] module signature defines a clock source, that is used with the
    Webmachine.Make(IO)(Clock) functor.

    Examples:

    (* static mock time *)
    module MockClock = struct
      let now = fun () -> 1526322704
    end

    (* using Unix.gettimeofday *)
    module UnixClock = struct
      let now = fun () -> int_of_float (Unix.gettimeofday ())
    end

    (* using Ptime_clock, which uses the system POSIX clock/gettimeofday *)
    module PtimeClock = struct
      let now = fun () ->
        int_of_float (Ptime.to_float (Ptime_clock.now ()))
    end

    (* using mirage-clock in MirageOS unikernels *)
    module MirageClock = struct
      let now = fun () ->
        let d, ps = Pclock.now_d_ps clock in
        let days_in_seconds  = d * 86_400 in
        let picos_in_seconds = Int64.(to_int (div ps (1_000_000_000_000L))) i
        days_in_seconds + picos_in_seconds
    end
*)
module type CLOCK = sig
  val now : unit -> int
end

module Make(IO:IO)(Clock:CLOCK) : S
  with type +'a io = 'a IO.t
