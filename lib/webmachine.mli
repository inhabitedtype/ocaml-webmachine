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
    module, and subclass the {{!classtype:S.resource}resouce} virtual class. *)

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

  type auth =
    [ `Authorized
    | `Basic of string
    | `Redirect of Uri.t
    ]

  val continue : 'a -> ('a, 'body) op
  (** [continue a rd] is equivalent to [IO.return (Ok x, rd)] *)

  val respond : ?body:'body -> int -> ('a, 'body) op
  (** [respond ?body n rd] is equivalent to [IO.return (Error n, { rd with resp_body = body }] *)

  class virtual ['body] resource : object
    constraint 'body = [> `Empty]

    method virtual content_types_provided : ((string * ('body provider)) list, 'body) op
    method virtual content_types_accepted : ((string * ('body acceptor)) list, 'body) op

    method resource_exists : (bool, 'body) op
    method service_available : (bool, 'body) op
    method is_authorized : (auth, 'body) op
    method forbidden : (bool, 'body) op
    method malformed_request : (bool, 'body) op
    method uri_too_long : (bool, 'body) op
    method known_content_type : (bool, 'body) op
    method valid_content_headers : (bool, 'body) op
    method valid_entity_length : (bool, 'body) op
    method options : ((string * string) list, 'body) op
    method allowed_methods : (Code.meth list, 'body) op
    method known_methods : (Code.meth list, 'body) op
    method delete_resource : (bool, 'body) op
    method delete_completed : (bool, 'body) op
    method process_post : (bool, 'body) op
    method language_available : (bool, 'body) op
    method charsets_provided : ((string * ('body -> 'body)) list, 'body) op
    method encodings_provided : ((string * ('body -> 'body)) list, 'body) op
    method variances : (string list, 'body) op
    method is_conflict : (bool, 'body) op
    method multiple_choices : (bool, 'body) op
    method previously_existed : (bool, 'body) op
    method moved_permanently : (Uri.t option, 'body) op
    method moved_temporarily : (Uri.t option, 'body) op
    method last_modified : (string option, 'body) op
    method expires : (string option, 'body) op
    method generate_etag : (string option, 'body) op
    method finish_request : (unit, 'body) op
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

        {[(pattern, exact, resource_constructor]}

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

module Make(IO:IO) : S
  with type +'a io = 'a IO.t
