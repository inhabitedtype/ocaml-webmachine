(*----------------------------------------------------------------------------
    Copyright (c) 2015 Inhabited Type LLC. All rights reserved.

    Proprietary and confidential.
  ----------------------------------------------------------------------------*)

open Cohttp

module type S = sig
  module IO : Cohttp.S.IO

  type 'body rd =
    { request : Request.t
    ; request_body : 'body
    ; response_headers : Header.t
    }

  type 'a result =
    | Ok of 'a
    | Error of int

  type ('a, 'body) op = 'body rd -> ('a result * 'body rd) IO.t
  type 'body provider = 'body rd -> ('body result * 'body rd) IO.t
  type 'body acceptor = (bool * 'body, 'body) op

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
    method delete_resource : ((bool * 'body), 'body) op
    method delete_completed : (bool, 'body) op
    method process_post : ((bool * 'body), 'body) op
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

  type 'body handler =
    body:'body -> request:Request.t -> (Code.status_code * Header.t * 'body * string list) IO.t

  val to_handler : resource:('body resource) -> 'body handler
  val dispatch : (string * 'body handler) list -> 'body handler
end

module Make(IO:Cohttp.S.IO) : S
  with module IO = IO
