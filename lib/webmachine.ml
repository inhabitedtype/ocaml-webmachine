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

open Cohttp

module type IO = sig
  type +'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Rd = Rd

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
  val respond : ?body:'body -> int -> ('a, 'body) op

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
    method post_is_create : (bool, 'body) op
    method create_path : (string, 'body) op
    method allow_missing_post : (bool, 'body) op
  end

  val to_handler
    :  ?dispatch_path:string
    -> ?path_info:(string * string) list
    -> resource:('body resource)
    -> body:'body
    -> request:Request.t
    -> unit
    -> (Code.status_code * Header.t * 'body * string list) io

  val dispatch
    :  ((Dispatch.tag * string) list * Dispatch.typ * (unit -> 'body resource)) list
    -> body:'body
    -> request:Request.t
    -> (Code.status_code * Header.t * 'body * string list) option io

  val dispatch'
    :  (string * (unit -> 'body resource)) list
    -> body:'body
    -> request:Request.t
    -> (Code.status_code * Header.t * 'body * string list) option io
end

let default_variances =
  [ "Accept"; "Accept-Encoding"; "Accept-Charset"; "Accept-Language"]

module type CLOCK = sig
  val now : unit -> int
end

module Make(IO:IO)(Clock:CLOCK) = struct
  type +'a io = 'a IO.t

  open IO

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

  let (>>=?) m f =
    m >>= function
    | Ok x, rd       -> f x rd
    | Error code, rd -> return (Error code, rd)

  let continue x rd = return (Ok x, rd)

  let respond ?body x rd =
    let rd =
      match body with
      | None           -> rd
      | Some resp_body -> { rd with Rd.resp_body }
    in
    return (Error x, rd)

  class virtual ['body] resource = object(self)
    constraint 'body = [> `Empty]

    method virtual content_types_provided : ((string * ('body provider)) list, 'body) op
    method virtual content_types_accepted : ((string * ('body acceptor)) list, 'body) op

    method resource_exists (rd:'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method service_available (rd:'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method is_authorized (rd :'body Rd.t) : (auth result * 'body Rd.t) IO.t =
      continue `Authorized rd
    method forbidden (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method malformed_request (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method uri_too_long (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method known_content_type (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method valid_content_headers (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method valid_entity_length (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method options (rd :'body Rd.t) : ((string * string) list result * 'body Rd.t) IO.t =
      self#allowed_methods rd >>=? fun meths rd ->
      continue ["allow", String.concat "," (List.map Code.string_of_method meths)] rd
    method allowed_methods (rd :'body Rd.t) : (Code.meth list result * 'body Rd.t) IO.t =
      continue [ `GET; `HEAD ] rd
    method known_methods (rd :'body Rd.t) : (Code.meth list result * 'body Rd.t) IO.t =
      continue [`GET; `HEAD; `POST; `PUT; `DELETE; `Other "TRACE"; `Other "CONNECT"; `OPTIONS] rd
    method delete_resource (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method delete_completed (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method process_post (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method language_available (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue true rd
    method charsets_provided (rd :'body Rd.t) : ((string * ('body -> 'body)) list result * 'body Rd.t) IO.t =
      continue [] rd
    method encodings_provided (rd :'body Rd.t) : ((string * ('body -> 'body)) list result * 'body Rd.t) IO.t =
      continue ["identity", fun x -> x] rd
    method variances (rd :'body Rd.t) : (string list result * 'body Rd.t) IO.t =
      continue [] rd
    method is_conflict (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method multiple_choices (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method previously_existed (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method moved_permanently (rd :'body Rd.t) : (Uri.t option result * 'body Rd.t) IO.t =
      continue None rd
    method moved_temporarily (rd :'body Rd.t) : (Uri.t option result * 'body Rd.t) IO.t =
      continue None rd
    method last_modified (rd :'body Rd.t) : (string option result * 'body Rd.t) IO.t =
      continue None rd
    method expires (rd :'body Rd.t) : (string option result * 'body Rd.t) IO.t =
      continue None rd
    method generate_etag (rd :'body Rd.t) : (string option result * 'body Rd.t) IO.t =
      continue None rd
    method finish_request (rd :'body Rd.t) : (unit result * 'body Rd.t) IO.t =
      continue () rd
    method post_is_create (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
    method create_path (rd :'body Rd.t) : (string result * 'body Rd.t) IO.t =
      continue "" rd
    method allow_missing_post (rd :'body Rd.t) : (bool result * 'body Rd.t) IO.t =
      continue false rd
  end

  let (>>~) m f = m f

  (* Decision State *)
  module DS = struct

    type 'body t =
      { mutable path : string list
      ; mutable rd : 'body Rd.t
      ; mutable content_type : (string * 'body provider) option
      ; mutable charset : (string * ('body -> 'body))  option
      ; mutable encoding : (string * ('body -> 'body)) option
      }

    let create rd =
      { path = []
      ; rd
      ; content_type = None
      ; charset = None
      ; encoding = None
      }
  end

  class ['body] logic ~(resource:'body resource) ~rd:(initial_rd:'body Rd.t) () = object(self)
    constraint 'body = [> `Empty]

    val state = DS.create initial_rd

    method private encode_body =
      let cf =
        match state.charset with
        | None        -> fun x -> x
        | Some (_, f) ->  f
      in
      let ef =
        match state.encoding with
        | None        -> fun x -> x
        | Some (_, f) -> f
      in
      state.rd <- { state.rd with Rd.resp_body = ef (cf state.rd.resp_body) }

    method private set_response_header k v =
      state.rd <- Rd.with_resp_headers (fun headers -> Header.replace headers k v) state.rd

    method private get_request_header k =
      Header.get state.rd.req_headers k

    method private get_response_header k =
      Header.get state.rd.resp_headers k

    method private respond ~status () : (Code.status_code * Header.t * 'body) IO.t =
      self#run_op resource#finish_request
      >>~ fun () -> return (status, state.rd.resp_headers, state.rd.resp_body)

    method private halt code : (Code.status_code * Header.t * 'body) IO.t =
      let status = Code.status_of_code code in
      self#respond ~status ()

    method private choose_charset acceptable k =
      (* XXX(seliopou): This breaks the {run_op} so watch out in the even that
       * this, or {run_op} must change behavior in order to keep them
       * consistent. *)
      resource#charsets_provided state.rd
      >>= function
        | Ok [], rd' ->
          state.rd <- rd'; k`Any
        | Ok available, rd' ->
          state.rd <- rd';
          state.charset <- Encoding.choose_charset ~available ~acceptable;
          k (`One state.charset)
        | Error n, rd' ->
          state.rd <- rd';
          self#halt n

    method private choose_encoding acceptable k =
      resource#encodings_provided state.rd
      >>= function
        | Ok available, rd' ->
          state.rd <- rd';
          state.encoding <- Encoding.choose ~available ~acceptable;
          k state.encoding
        | Error n, rd' ->
          state.rd <- rd';
          self#halt n

    (** [run_op op] runs [op] with the current request and response
        information, and will perform any appropriate bookkeeping that needs to
        be done given the result. *)
    method private run_op : 'a. ('a, 'body) op -> ('a -> (Code.status_code * Header.t * 'body) IO.t) -> (Code.status_code * Header.t * 'body) IO.t =
      fun op k -> op state.rd
        >>= function
          | Ok a, rd' ->
            state.rd <- rd';
            k a
          | Error n, rd' ->
            state.rd <- rd';
            self#halt n

    method private run_provider : 'body provider -> _ -> (Code.status_code * Header.t * 'body) IO.t =
      fun provider k ->
        provider state.rd
        >>= function
          | Ok resp_body, rd' ->
            state.rd <- { rd' with Rd.resp_body };
            k ()
          | Error n , rd' ->
            state.rd <- rd';
            self#halt n

    method private accept_helper k =
      let header =
        match self#get_request_header "content-type" with
        | None       -> Some "application/octet-stream"
        | Some type_ -> Some type_
      in
      self#run_op resource#content_types_accepted
      >>~ fun provided ->
        match Mediatype.match_header provided header with
        | None                -> self#halt 415
        | Some(_, of_content) ->
          self#run_op of_content
          >>~ function complete ->
            if complete then
              self#encode_body;
            k complete

    method private d n =
      state.path <- n :: state.path

    method run : (Code.status_code * Header.t * 'body * string list) IO.t =
      self#v3b13 >>= fun (code, headers, body) -> return (code, headers, body, List.rev state.path)

    method v3b13 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b13";
      self#run_op resource#service_available
      >>~ function
        | true  -> self#v3b12
        | false -> self#halt 503

    method v3b12 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b12";
      self#run_op resource#known_methods
      >>~ fun (meths:Code.meth list) ->
        if List.exists (fun x -> Code.compare_method state.rd.meth x = 0) meths
        then self#v3b11
        else self#halt 501

    method v3b11 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b11";
      self#run_op resource#uri_too_long
      >>~ function
        | true  -> self#halt 414
        | false -> self#v3b10

    method v3b10 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b10";
      self#run_op resource#allowed_methods
      >>~ fun (meths:Code.meth list) ->
        if List.exists (fun x -> Code.compare_method state.rd.meth x = 0) meths
        then self#v3b9
        else (
          let allow = String.concat "," (List.map Code.string_of_method meths) in
          self#set_response_header "allow" allow;
          self#halt 405)

    method v3b9 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b9";
      self#run_op resource#malformed_request
      >>~ function
        | true  -> self#halt 400
        | false -> self#v3b8

    method v3b8 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b8";
      self#run_op resource#is_authorized
      >>~ function
        | `Authorized -> self#v3b7
        | `Basic realm ->
          self#set_response_header "WWW-Authenticate" ("Basic realm=\"" ^ realm ^ "\"");
          self#halt 401
        | `Challenge auth ->
          let challenge =
            let buffer = Buffer.create 80 in
            let add_kv (k, v) =
              Buffer.add_char buffer ' ';
              Buffer.add_string buffer k;
              Buffer.add_string buffer "=\"";
              Buffer.add_string buffer v;
              Buffer.add_string buffer "\"";
            in
            Buffer.add_string buffer auth.scheme;
            add_kv ("realm", auth.realm);
            List.iter add_kv auth.params;
            Buffer.contents buffer
          in
          self#set_response_header "WWW-Authenticate" challenge;
          self#halt 401
        | `Redirect uri ->
          state.rd <- Rd.redirect Uri.(to_string uri) state.rd;
          self#halt 303

    method v3b7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b7";
      self#run_op resource#forbidden
      >>~ function
        | true  -> self#halt 403
        | false -> self#v3b6

    method v3b6 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b6";
      self#run_op resource#valid_content_headers
      >>~ function
        | true  -> self#v3b5
        | false -> self#halt 501

    method v3b5 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b5";
      self#run_op resource#known_content_type
      >>~ function
        | true  -> self#v3b4
        | false -> self#halt 415

    method v3b4 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b4";
      self#run_op resource#valid_entity_length
      >>~ function
        | true  -> self#v3b3
        | false -> self#halt 413

    method v3b3 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3b3";
      match state.rd.meth with
      | `OPTIONS ->
        self#run_op resource#options
        >>~ fun headers ->
          List.iter (fun (k, v) -> self#set_response_header k v) headers;
          self#respond ~status:`OK ()
      | _ -> self#v3c3

    method v3c3 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3c3";
      self#run_op resource#content_types_provided
      >>~ fun content_types ->
        match self#get_request_header "accept" with
        | None   ->
          begin match content_types with
          | []   -> self#halt 500
          | t::_ ->
            state.content_type <- Some t;
            self#v3d4
          end
        | Some _ -> self#v3c4

    method v3c4 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3c4";
      self#run_op resource#content_types_provided
      >>~ fun content_types ->
        let header = self#get_request_header "accept" in
        match Mediatype.match_header content_types header with
        | None   -> self#halt 406
        | Some t ->
          state.content_type <- Some t;
          self#v3d4

    method v3d4 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3d4";
      match self#get_request_header "accept-language" with
      | None   -> self#v3e5
      | Some _ -> self#v3d5

    method v3d5 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3d5";
      self#run_op resource#language_available
      >>~ function
        | true  -> self#v3e5
        | false -> self#halt 406

    method v3e5 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3e5";
      match self#get_request_header "accept-charset" with
      | None   ->
        begin self#choose_charset (Accept.charsets None)
        >>~ function
          | `Any
          | `One (Some _) -> self#v3f6
          | `One None     -> self#halt 406
        end
      | Some _ -> self#v3e6

    method v3e6 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3e6";
      match self#get_request_header "accept-charset" with
      | None            -> assert false
      | Some acceptable ->
        begin self#choose_charset (Accept.charsets (Some acceptable))
        >>~ function
          | `Any
          | `One (Some _) -> self#v3f6
          | `One None     -> self#halt 406
        end

    method v3f6 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3f6";
      let type_ =
        match state.content_type with
        | None            -> assert false
        | Some (type_, _) -> type_
      in
      let value =
        match state.charset with
        | None             -> type_
        | Some (charset,_) -> Printf.sprintf "%s; charset=%s" type_ charset
      in
      self#set_response_header "Content-Type" value;
      match self#get_request_header "accept-encoding" with
      | None ->
        let acceptable = Accept.encodings (Some "identity;q=1.0,*;q=0.5") in
        self#choose_encoding acceptable >>~ fun chosen ->
        begin match chosen with
        | None   -> self#halt 406
        | Some _ -> self#v3g7
        end
      | Some _ -> self#v3f7

    method v3f7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3f7";
      match self#get_request_header "accept-encoding" with
      | None            -> assert false
      | Some acceptable ->
        let acceptable = Accept.encodings (Some acceptable) in
        self#choose_encoding acceptable >>~ fun chosen ->
        begin match chosen with
        | None   -> self#halt 406
        | Some _ -> self#v3g7
        end

    method v3g7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3g7";
      self#run_op resource#variances >>~ fun variances ->
      let variances = variances @ default_variances in
      begin match String.concat ", " variances with
      | ""   -> ()
      | vary -> self#set_response_header "vary" vary
      end;
      self#run_op resource#resource_exists
      >>~ function
        | true  -> self#v3g8
        | false -> self#v3h7

    method v3g8 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3g8";
      match self#get_request_header "if-match" with
      | None   -> self#v3h10
      | Some _ -> self#v3g9

    method v3g9 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3g9";
      match self#get_request_header "if-match" with
      | None     -> assert false
      | Some "*" -> self#v3h10
      | Some _   -> self#v3g11

    method v3g11 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3g11";
      match self#get_request_header "if-match" with
      | None      -> assert false
      | Some if_match_header ->
        self#run_op resource#generate_etag
        >>~ function
        | None -> self#halt 412
        | Some etag ->
          begin match List.mem etag (Etag.from_header if_match_header) with
          | true  -> self#v3h10
          | false -> self#halt 412
          end

    method v3h7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3h7";
      match self#get_request_header "if-match" with
      | None   -> self#v3i7
      | Some _ -> self#halt 412

    method v3h10 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3h10";
      match self#get_request_header "if-unmodified-since" with
      | None   -> self#v3i12
      | Some _ -> self#v3h11

    method v3h11 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3h11";
      let d = self#get_request_header "if-unmodified-since" in
      match d with
      | None -> self#v3i12
      | Some d' ->
         match (Rfc1123.parse_date d') with
         | None -> self#v3i12
         | Some _ -> self#v3h12

    method v3h12 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3h12";
      try
        let u_mod = self#get_request_header "if-unmodified-since" in
        self#run_op resource#last_modified
        >>~ fun l_mod ->
        match (u_mod, l_mod) with
        | (Some u_mod', Some l_mod') ->
           (match (Rfc1123.parse_date_exn l_mod') > (Rfc1123.parse_date_exn u_mod') with
           | false -> self#v3i12
           | true -> self#halt 412)
        | (_, _) -> self#v3i12
      with
        Invalid_argument _ -> self#halt 412

    method v3i4 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3i4";
      self#run_op resource#moved_permanently
      >>~ function
        | None     -> self#v3p3
        | Some uri ->
          self#set_response_header "Location" (Uri.to_string uri);
          self#respond ~status:`Moved_permanently ()

    method v3i7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3i7";
      match state.rd.meth with
      | `OPTIONS -> assert false
      | `PUT     -> self#v3i4
      | _        -> self#v3k7

    method v3i12 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3i12";
      match self#get_request_header "if-none-match" with
      | None   -> self#v3l13
      | Some _ -> self#v3i13

    method v3i13 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3i13";
      match self#get_request_header "if-none-match" with
      | None     -> assert false
      | Some "*" -> self#v3j18
      | Some _   -> self#v3k13

    method v3k7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3k7";
      self#run_op resource#previously_existed
      >>~ function
        | true  -> self#v3k5
        | false -> self#v3l7

    method v3k5 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3k5";
      self#run_op resource#moved_permanently
      >>~ function
        | None     -> self#v3l5
        | Some uri ->
          self#set_response_header "location" (Uri.to_string uri);
          self#respond ~status:`Moved_permanently ()

    method v3k13 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3k13";
      match self#get_request_header "if-none-match" with
      | None      -> assert false
      | Some if_none_match_header ->
        self#run_op resource#generate_etag
        >>~ function
        | None -> self#v3l13
        | Some etag ->
          begin match List.mem etag (Etag.from_header if_none_match_header) with
          | true  -> self#v3j18
          | false -> self#v3l13
          end

    method v3l5 : (Code.status_code * Header.t * 'body) IO.t =
      (* XXX(seliopou): For now, no POSTs to non-existent resources allowed. *)
      self#d "v3l5";
      self#run_op resource#moved_temporarily
      >>~ function
        | None     -> self#v3m5
        | Some uri ->
          self#set_response_header "location" (Uri.to_string uri);
          self#respond ~status:`Temporary_redirect ()

    method v3l7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3l7";
      match state.rd.meth with
      | `POST -> self#v3m7
      | _     -> self#halt 404

    method v3l13 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3l13";
      match self#get_request_header "if-modified-since" with
      | None   -> self#v3m16
      | Some _ -> self#v3l14

    method v3l14 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3l14";
      match (self#get_request_header "if-modified-since") with
      | None -> self#v3m16
      | Some date ->
         match (Rfc1123.parse_date date) with
         | Some _ -> self#v3l15
         | None ->  self#v3m16

    method v3l15 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3l15";
      let now = Clock.now () in
      match (self#get_request_header "if-modified-since") with
      | None -> self#v3l17
      | Some date ->
         match Rfc1123.parse_date date with
         | None -> self#v3l17
         | Some d -> match (d > now) with
                     | true -> self#v3m16
                     | false -> self#v3l17

    method v3l17 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3l17";
      try
        let u_mod = self#get_request_header "if-modified-since" in
        self#run_op resource#last_modified
        >>~ fun l_mod ->
            match (u_mod, l_mod) with
            | (Some l_mod', Some u_mod') ->
               (match (Rfc1123.parse_date_exn l_mod') > (Rfc1123.parse_date_exn u_mod') with
                | true -> self#v3m16
                | false -> self#halt 304)
            | (_, _) -> self#halt 304
      with
        Invalid_argument _ -> self#halt 304

    method v3j18 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3j18";
      match state.rd.meth with
      | `GET | `HEAD -> self#halt 304
      | _            -> self#halt 412

    method v3m5 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3m5";
      match state.rd.meth with
      | `POST -> self#v3n5
      | _     -> self#halt 410

    method v3m7 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3m7";
      self#run_op resource#allow_missing_post
      >>~ function
        | true  -> self#v3n11
        | false -> self#halt 404

    method v3m16 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3m16";
      match state.rd.meth with
      | `OPTIONS -> assert false
      | `DELETE  -> self#v3m20
      | _        -> self#v3n16

    method v3m20 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3m20";
      self#run_op resource#delete_resource
      >>~ fun deleted ->
        if deleted then
          self#run_op resource#delete_completed
          >>~ function
            | true  -> self#v3o20
            | false -> self#respond ~status:`Accepted ()
        else
          self#halt 500

    method v3n5 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3n5";
      self#run_op resource#allow_missing_post
      >>~ function
        | true  -> self#v3n11
        | false -> self#halt 410

    method v3n11 : (Code.status_code * Header.t * 'body) IO.t =
      let stage2 (type a) (_ : a) =
        if state.rd.resp_redirect
        then
          (match self#get_response_header "location" with
           | None   -> self#halt 500
           | Some _ -> self#respond ~status:`See_other ())
        else self#v3p11
      in
      self#d "v3n11";
      self#run_op resource#post_is_create >>~ function
      | true ->
        self#run_op resource#create_path >>~ fun new_resource ->
        (* get full path, based on base uri *)
        (* set disp path on rd *)
        let uri = state.rd.uri in
        let uri' =
          Uri.with_path uri (Uri.path uri ^ "/" ^ new_resource)
        in
        (* set location header on rd *)
        self#set_response_header "Location" (Uri.to_string uri');
        self#accept_helper stage2
      | false ->
        self#run_op resource#process_post >>~ fun executed ->
        if executed
        then begin
          self#encode_body;
          stage2 ()
        end
        else self#halt 500

    method v3n16 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3n16";
      match state.rd.meth with
      | `OPTIONS | `DELETE -> assert false
      | `POST -> self#v3n11
      | _     -> self#v3o16

    method v3o14 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3o14";
      self#run_op resource#is_conflict
      >>~ function
        | true  -> self#halt 409
        | false -> self#accept_helper (fun _ -> self#v3p11)

    method v3o16 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3o16";
      match state.rd.meth with
      | `OPTIONS | `DELETE | `POST -> assert false
      | `PUT -> self#v3o14
      | _    -> self#v3o18

    method v3o18 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3o18";
      match state.rd.meth with
      (* The HTTP method could be POST if the request comes via v3o20 *)
      | `OPTIONS     -> assert false
      | `HEAD | `GET ->
        let _, to_content =
          match state.content_type with
          | None   -> assert false
          | Some x -> x
        in
        self#run_op resource#generate_etag >>~ fun etag ->
          begin match etag with
          | None -> ()
          | Some etag -> self#set_response_header "ETag" (Etag.escape etag)
          end;
          (* XXX(seliopou) last modified *)
          (* XXX(seliopou) expires *)
          self#run_provider to_content >>~ fun () ->
          self#encode_body;
          self#v3o18b
      | _ ->
        self#v3o18b

    method v3o18b :(Code.status_code * Header.t * 'body) IO.t =
      self#run_op resource#multiple_choices
      >>~ function
        | true  -> self#halt 300
        | false -> self#respond ~status:`OK ()

    method v3o20 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3o20";
      match state.rd.resp_body with
      | `Empty -> self#respond ~status:`No_content ()
      | _      -> self#v3o18

    method v3p3 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3p3";
      self#run_op resource#is_conflict
      >>~ function
        | true  -> self#halt 409
        | false -> self#accept_helper (fun _ -> self#v3p11)

    method v3p11 : (Code.status_code * Header.t * 'body) IO.t =
      self#d "v3p11";
      match self#get_response_header "location" with
      | None   -> self#v3o20
      | Some _ -> self#respond ~status:`Created ()
  end

  let to_handler ?dispatch_path ?path_info ~resource ~body ~request () =
    let rd = Rd.make ~req_body:body ?dispatch_path ?path_info ~request () in
    let logic = new logic ~resource ~rd () in
    logic#run
  ;;

  let dispatch table =
    let table =
      Dispatch.create
        (List.map (fun (p, t, mk_resource) ->
             (p, t, fun path_info dispatch_path ~body ~request ->
                 let resource = mk_resource () in
                 to_handler ?dispatch_path ~path_info ~resource ~body ~request ()))
            table)
    in
    fun ~body ~request ->
      let path = Uri.path (Cohttp.Request.uri request) in
      match Dispatch.dispatch table path with
      | None -> return None
      | Some handler -> handler ~body ~request >>= fun x -> return (Some x)

  let dispatch' table =
    dispatch (List.map (fun (m, r) ->
      let p, t = Dispatch.of_dsl m in
      (p, t, r))
    table)
end
