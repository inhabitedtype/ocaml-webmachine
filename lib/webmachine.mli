(*----------------------------------------------------------------------------
    Copyright (c) 2015 Inhabited Type LLC. All rights reserved.

    Proprietary and confidential.
  ----------------------------------------------------------------------------*)

open Cohttp

class type ['body] rd = object
  constraint 'body = [> `Empty]

  method meth : Code.meth
  method version : Code.version
  method uri : Uri.t

  method req_headers : Header.t
  method req_body : 'body
  method resp_headers : Header.t
  method resp_body : 'body

  method set_req_body : 'body -> 'body rd
  method set_req_headers : Header.t -> 'body rd
  method set_resp_body : 'body -> 'body rd
  method set_resp_headers : Header.t -> 'body rd

  method disp_path : string
  method path_info : string -> string option
  method path_info_exn : string -> string
end

module type S = sig
  module IO : Cohttp.S.IO

  type 'a result =
    | Ok of 'a
    | Error of int

  type ('a, 'body) op = 'body rd -> ('a result * 'body rd) IO.t
  type 'body provider = ('body, 'body) op
  type 'body acceptor = (bool, 'body) op

  val continue : 'a -> ('a, 'body) op
  val respond : ?body:'body -> int -> ('a, 'body) op

  class virtual ['body] resource : object
    constraint 'body = [> `Empty]

    method virtual content_types_provided : ((string * ('body provider)) list, 'body) op
    method virtual content_types_accepted : ((string * ('body acceptor)) list, 'body) op

    method resource_exists : (bool, 'body) op
    method service_available : (bool, 'body) op
    method auth_required : (bool, 'body) op
    method is_authorized : (bool, 'body) op
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
    resource:('body resource) -> body:'body -> request:Request.t ->
    (Code.status_code * Header.t * 'body * string list) IO.t

  val dispatch' :
    (string * (unit -> 'body resource)) list ->
    body:'body -> request:Request.t ->
    (Code.status_code * Header.t * 'body * string list) option IO.t

  val dispatch :
    ([`M of string | `L of string] list * bool * (unit -> 'body resource)) list ->
    body:'body -> request:Request.t ->
    (Code.status_code * Header.t * 'body * string list) option IO.t
end

module Make(IO:Cohttp.S.IO) : S
  with module IO = IO
