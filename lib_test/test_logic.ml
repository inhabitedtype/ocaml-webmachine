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
[@@@ocaml.warning "-7"]

module Id = struct
  type +'a t = Id of 'a

  let (>>=) (Id a) f = f a
  let return a = Id a
  let run (Id a) = a

end

module ClockMock = struct
  let now = fun () -> 1526322704
end

module Webmachine = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Id)(ClockMock)
end

let run = Id.run

open Cohttp

let http_1_0_methods = [`GET; `POST; `HEAD]
let http_1_1_methods =
  [`GET; `HEAD; `POST; `PUT; `DELETE; `Other "TRACE"; `Other "CONNECT"; `OPTIONS]
let default_allowed_methods = [`GET; `HEAD; `PUT]

let to_html rd = Webmachine.continue (`String "<html><body>Foo</body></html>") rd
let of_plain rd = Webmachine.continue true rd
let of_plain_with_loc loc rd =
  Webmachine.continue true Webmachine.Rd.(with_resp_headers (fun header ->
    Header.add header "location" loc) rd)


class test_resource = object
  (* A configurable resource for testing. Every method on the resource has a
   * result that's determined by an internal variable that you can set. For
   * example, to set the value that [r#is_authorized] will return, simply call
   * [r#set_is_authorized false].
   *)

  inherit [Cohttp.Body.t] Webmachine.resource

  val _content_types_provided = ref ["text/html", to_html]
  val _content_types_accepted = ref []

  method content_types_provided rd =
    Webmachine.continue !_content_types_provided rd
  method content_types_accepted rd =
    Webmachine.continue !_content_types_accepted rd

  method set_content_types_provided v =
    _content_types_provided := v
  method set_content_types_accepted v =
    _content_types_accepted := v

  val _resource_exits = ref true
  val _service_available = ref true
  val _is_authorized = ref `Authorized
  val _forbidden = ref false
  val _malformed_request = ref false
  val _uri_too_long = ref false
  val _known_content_type = ref true
  val _valid_content_headers = ref true
  val _valid_entity_length = ref true
  val _options = ref []
  val _allowed_methods = ref [`GET; `HEAD]
  val _known_methods = ref [`GET; `HEAD; `POST; `PUT; `DELETE; `Other "TRACE"; `Other "CONNECT"; `OPTIONS]
  val _delete_resource = ref false
  val _delete_completed = ref true
  val _process_post = ref false
  val _language_available = ref true
  val _charsets_provided = ref []
  val _encodings_provided = ref ["identity", fun x -> x]
  val _variances = ref []
  val _is_conflict = ref false
  val _multiple_choices = ref false
  val _previously_existed = ref false
  val _moved_permanently = ref None
  val _moved_temporarily = ref None
  val _last_modified = ref None
  val _expires = ref None
  val _generate_etag = ref None
  val _finish_request = ref ()
  val _post_is_create = ref false
  val _create_path = ref ""

  method resource_exists rd =
    Webmachine.continue !_resource_exits rd
  method service_available rd =
    Webmachine.continue !_service_available rd
  method is_authorized rd =
    Webmachine.continue !_is_authorized rd
  method forbidden rd =
    Webmachine.continue !_forbidden rd
  method malformed_request rd =
    Webmachine.continue !_malformed_request rd
  method uri_too_long rd =
    Webmachine.continue !_uri_too_long rd
  method known_content_type rd =
    Webmachine.continue !_known_content_type rd
  method valid_content_headers rd =
    Webmachine.continue !_valid_content_headers rd
  method valid_entity_length rd =
    Webmachine.continue !_valid_entity_length rd
  method options rd =
    Webmachine.continue !_options rd
  method allowed_methods rd =
    Webmachine.continue !_allowed_methods rd
  method known_methods rd =
    Webmachine.continue !_known_methods rd
  method delete_resource rd =
    Webmachine.continue !_delete_resource rd
  method delete_completed rd =
    Webmachine.continue !_delete_completed rd
  method process_post rd =
    Webmachine.continue !_process_post rd
  method language_available rd =
    Webmachine.continue !_language_available rd
  method charsets_provided rd =
    Webmachine.continue !_charsets_provided rd
  method encodings_provided rd =
    Webmachine.continue !_encodings_provided rd
  method variances rd =
    Webmachine.continue !_variances rd
  method is_conflict rd =
    Webmachine.continue !_is_conflict rd
  method multiple_choices rd =
    Webmachine.continue !_multiple_choices rd
  method previously_existed rd =
    Webmachine.continue !_previously_existed rd
  method moved_permanently rd =
    Webmachine.continue !_moved_permanently rd
  method moved_temporarily rd =
    Webmachine.continue !_moved_temporarily rd
  method last_modified rd =
    Webmachine.continue !_last_modified rd
  method expires rd =
    Webmachine.continue !_expires rd
  method generate_etag rd =
    Webmachine.continue !_generate_etag rd
  method finish_request rd =
    Webmachine.continue !_finish_request rd
  method post_is_create rd =
    Webmachine.continue !_post_is_create rd
  method create_path rd =
    Webmachine.continue !_create_path rd

  method set_resource_exists v =
    _resource_exits := v
  method set_service_available v =
    _service_available := v
  method set_is_authorized v =
    _is_authorized := v
  method set_forbidden v =
    _forbidden := v
  method set_malformed_request v =
    _malformed_request := v
  method set_uri_too_long v =
    _uri_too_long := v
  method set_known_content_type v =
    _known_content_type := v
  method set_valid_content_headers v =
    _valid_content_headers := v
  method set_valid_entity_length v =
    _valid_entity_length := v
  method set_options v =
    _options := v
  method set_allowed_methods v =
    _allowed_methods := v
  method set_known_methods v =
    _known_methods := v
  method set_delete_resource v =
    _delete_resource := v
  method set_delete_completed v =
    _delete_completed := v
  method set_process_post v =
    _process_post := v
  method set_language_available v =
    _language_available := v
  method set_charsets_provided v =
    _charsets_provided := v
  method set_encodings_provided v =
    _encodings_provided := v
  method set_variances v =
    _variances := v
  method set_is_conflict v =
    _is_conflict := v
  method set_multiple_choices v =
    _multiple_choices := v
  method set_previously_existed v =
    _previously_existed := v
  method set_moved_permanently v =
    _moved_permanently := v
  method set_moved_temporarily v =
    _moved_temporarily := v
  method set_last_modified v =
    _last_modified := v
  method set_expires v =
    _expires := v
  method set_generate_etag v =
    _generate_etag := v
  method set_finish_request v =
    _finish_request := v
  method set_post_is_create v =
    _post_is_create := v
  method set_create_path v =
    _create_path := v

end

open OUnit

module Path = struct
  let to_b13 = ["v3b13"]
  let to_b12 = to_b13 @ ["v3b12"]
  let to_b11 = to_b12 @ ["v3b11"]
  let to_b10 = to_b11 @ ["v3b10"]
  let to_b9 = to_b10 @ ["v3b9"]
  let to_b8 = to_b9 @ ["v3b8"]
  let to_b7 = to_b8 @ ["v3b7"]
  let to_b6 = to_b7 @ ["v3b6"]
  let to_b5 = to_b6 @ ["v3b5"]
  let to_b4 = to_b5 @ ["v3b4"]
  let to_b3 = to_b4 @ ["v3b3"]

  (* c3 - there is one path to state c3 *)
  let to_c3 = to_b3 @ ["v3c3"]

  (* c4 - there is one path to state c4 *)
  let to_c4 = to_c3 @ ["v3c4"]

  (* d4 - there are two paths to d4: via c3 or via c4 *)
  let to_d4_via_c3 = to_c3 @ ["v3d4"]
  let to_d4_via_c4 = to_c4 @ ["v3d4"]

  (* d5 - there are two paths to d5: via c3 or via c4 *)
  let to_d5_via_c3 = to_d4_via_c3 @ ["v3d5"]
  let to_d5_via_c4 = to_d4_via_c4 @ ["v3d5"]

  (* e5 - there are four paths to e5: via d5 (via c3 or via c4) or via d4 (via c3
   * or via c4 only some of these paths are tested. *)
  let to_e5_via_d5_c3 = to_d5_via_c3 @ ["v3e5"]
  let to_e5_via_d5_c4 = to_d5_via_c4 @ ["v3e5"]
  let to_e5_via_d4_c3 = to_d4_via_c3 @ ["v3e5"]

  (* e6 - there are four paths to e6: via d5 (via c3 or via c4) or via d4 (via c3
   * or via c4 only two of these paths to e6 are tested *)
  let to_e6_via_d5_c3 = to_e5_via_d5_c3 @ ["v3e6"]
  let to_e6_via_d5_c4 = to_e5_via_d5_c4 @ ["v3e6"]

  (* f6 - selection of the paths to f6 *)
  let to_f6_via_e6_d5_c4 = to_e6_via_d5_c4 @ ["v3f6"]
  let to_f6_via_e5_d4_c3 = to_e5_via_d4_c3 @ ["v3f6"]

  (* f7 - a path to f7 *)
  let to_f7_via_e6_d5_c4 = to_f6_via_e6_d5_c4 @ ["v3f7"]

  (* g7 - the path to g7 = without accept headers in the request *)
  let to_g7_via_f6_e6_d5_c4 = to_f6_via_e5_d4_c3 @ ["v3g7"]
  let to_g7_no_acpthead = to_g7_via_f6_e6_d5_c4

  (* g9 - the path to g9 = without accept headers in the request *)
  let to_g9_via_f6_e6_d5_c4 =
    to_g7_via_f6_e6_d5_c4 @ ["v3g8" ; "v3g9"]

  (* g11 - the path to g11 = without accept headers in the request *)
  let to_g11_via_f6_e6_d5_c4 =
    to_g7_via_f6_e6_d5_c4 @ ["v3g8"; "v3g9"; "v3g11"]
  let to_g11_no_acpthead = to_g11_via_f6_e6_d5_c4

  (* h7 - the path to h7 without accept headers *)
  let to_h7_no_acpthead = to_g7_no_acpthead @ ["v3h7"]

  (* i7 - the path to i7 without accept headers *)
  let to_i7_no_acpthead = to_h7_no_acpthead @ ["v3i7"]

  (* i4 - the path to i4 without accept headers *)
  let to_i4_no_acpthead = to_i7_no_acpthead @ ["v3i4"]

  (* k7 - the path to k7 without accept headers *)
  let to_k7_no_acpthead = to_i7_no_acpthead @ ["v3k7"]

  (* l7 - the path to l7 without accept headers *)
  let to_l7_no_acpthead = to_k7_no_acpthead @ ["v3l7"]

  (* m7 - the path to m7 without accept headers *)
  let to_m7_no_acpthead = to_l7_no_acpthead @ ["v3m7"]

  (* p3 - the path to p3 without accept headers *)
  let to_p3_no_acpthead = to_i4_no_acpthead @ ["v3p3"]

  (* k5 - the path to k5 without accept headers *)
  let to_k5_no_acpthead = to_k7_no_acpthead @ ["v3k5"]

  (* l5 - the path to l5 without accept headers *)
  let to_l5_no_acpthead = to_k5_no_acpthead @ ["v3l5"]

  (* m5 - the path to m5 without accept headers *)
  let to_m5_no_acpthead = to_l5_no_acpthead @ ["v3m5"]

  (* n5 - the path to n5 without accept headers *)
  let to_n5_no_acpthead = to_m5_no_acpthead @ ["v3n5"]

  (* h10 - the path to h10 without accept headers *)
  let to_h10_via_g8_f6_e6_d5_c4 =
    to_g7_via_f6_e6_d5_c4 @ ["v3g8"; "v3h10"]

  (* n11 - two paths to n11 without accept headers *)
  let to_n11_via_m7_no_acpthead = to_m7_no_acpthead @ ["v3n11"]
  let to_n11_via_n5_no_acpthead = to_n5_no_acpthead @ ["v3n11"]

  (* h11 - the path to h11 without accept headers = via g11 *)
  let to_h11_via_g11_f6_e6_d5_c4 =
    to_g11_no_acpthead @ ["v3h10"; "v3h11"]

  (* h12 - two paths to h12 without accept headers *)
  let to_h12_via_g8_f6_e6_d5_c4 =
    to_h10_via_g8_f6_e6_d5_c4 @ ["v3h11"; "v3h12"]
  let to_h12_via_g9_f6_e6_d5_c4 =
    to_g9_via_f6_e6_d5_c4 @ ["v3h10"; "v3h11"; "v3h12"]
  let to_h12_no_acpthead = to_h12_via_g8_f6_e6_d5_c4
  let to_h12_no_acpthead_2 = to_h12_via_g9_f6_e6_d5_c4

  (* i12 - two paths to i12 without accept headers *)
  let to_i12_via_h10_g8_f6_e6_d5_c4 =
    to_h10_via_g8_f6_e6_d5_c4 @ ["v3i12"]
  let to_i12_via_h11_g11_f6_e6_d5_c4 =
    to_h11_via_g11_f6_e6_d5_c4 @ ["v3i12"]

  (* l13 - a path to l13 without accept headers *)
  let to_l13_no_acpthead = to_i12_via_h10_g8_f6_e6_d5_c4 @ ["v3l13"]

  (* m16 - a path to m16 without accept headers *)
  let to_m16_no_acpthead = to_l13_no_acpthead @ ["v3m16"]

  (* m20 - a path to m20 without accept headers *)
  let to_m20_no_acpthead = to_m16_no_acpthead @ ["v3m20"]

  (* n16 - a path to n16 without accept headers *)
  let to_n16_no_acpthead = to_m16_no_acpthead @ ["v3n16"]

  (* o16 - a path to o16 without accept headers *)
  let to_o16_no_acpthead = to_n16_no_acpthead @ ["v3o16"]

  (* o14 - a path to o14 without accept headers *)
  let to_o14_no_acpthead = to_o16_no_acpthead @ ["v3o14"]

  (* o18 - a path to o18 without accept headers *)
  let to_o18_no_acpthead = to_o16_no_acpthead @ ["v3o18"]

  (* l17 - a path to l17 without accept headers *)
  let to_l17_no_acpthead =
    to_l13_no_acpthead @ ["v3l14"; "v3l15"; "v3l17"]

  (* i13 - two paths to i13 without accept headers *)
  let to_i13_via_h10_g8_f6_e6_d5_c4 =
    to_i12_via_h10_g8_f6_e6_d5_c4 @ ["v3i13"]
  let to_i13_via_h11_g11_f6_e6_d5_c4 =
    to_i12_via_h11_g11_f6_e6_d5_c4 @ ["v3i13"]

  (* k13 - the path to k13 without accept headers, via i13, i12, h11, g11 *)
  let to_k13_via_h11_g11_f6_e6_d5_c4 =
    to_i13_via_h11_g11_f6_e6_d5_c4 @ ["v3k13"]

  (* j18 - three paths to j18 without accept headers (one via h10; one via h11
   * and k13; one via h12) *)
  let to_j18_via_i13_h10_g8_f6_e6_d5_c4 =
    to_i13_via_h10_g8_f6_e6_d5_c4 @ ["v3j18"]
  let to_j18_via_k13_i13_h10_g8_f6_e6_d5_c4 =
    to_i13_via_h10_g8_f6_e6_d5_c4 @ ["v3k13";"v3j18"]
  let to_j18_via_k13_h11_g11_f6_e6_d5_c4 =
    to_k13_via_h11_g11_f6_e6_d5_c4 @ ["v3j18"]
  let to_j18_no_acpthead = to_j18_via_i13_h10_g8_f6_e6_d5_c4
  let to_j18_no_acpthead_2 = to_j18_via_k13_h11_g11_f6_e6_d5_c4
  let to_j18_no_acpthead_3 =
    to_h12_no_acpthead_2 @ ["v3i12"; "v3i13"; "v3j18"]

  (* p11 - three paths to p11 without accept headers, via n11, p3, or o14 *)
  let to_p11_via_n11_no_acpthead =
    to_n11_via_m7_no_acpthead @ ["v3p11"]
  let to_p11_via_p3_no_acpthead = to_p3_no_acpthead @ ["v3p11"]
  let to_p11_via_o14_no_acpthead = to_o14_no_acpthead @ ["v3p11"]

  (* o20 - the path to o20 via p11 via o14 *)
  let to_o20_via_p11_via_o14_no_acpthead =
    to_p11_via_o14_no_acpthead @ ["v3o20"]

  (* o18 *)
  let to_o18_no_acpthead_via_ifmatch =
    to_k13_via_h11_g11_f6_e6_d5_c4 @ ["v3l13";"v3m16";"v3n16";"v3o16";"v3o18"]
end

let with_test_resource f =
  let resource, logic =
    let resource = new test_resource in
    let logic = Webmachine.to_handler ~resource:(resource :> [> `Empty] Webmachine.resource) in
    resource, (fun request -> run (logic ~body:`Empty ~request ()))
  in
  logic (f resource)
;;

let with_test_resource' f =
  let resource, logic =
    let resource = new test_resource in
    let logic = Webmachine.to_handler ~resource:(resource :> [> `Empty] Webmachine.resource) in
    resource, (fun (request, body) -> run (logic ~body ~request ()))
  in
  logic (f resource)
;;

let assert_status ~msg (status_code, _, _, _) status =
  assert_equal ~msg ~printer:string_of_int
    status Code.(code_of_status status_code)
;;

let assert_path ~msg (_, _, _, p1) p2 =
  let printer s = Printf.sprintf "[%s]" (String.concat "; " s) in
  assert_equal ~msg ~printer p2 p1
;;

let assert_header ~msg (_, headers, _, _) header value =
  let printer = function None -> "<none>" | Some x -> x in
  assert_equal ~msg ~printer (Header.get headers header) (Some value)
;;

(* BEGIN TESTS *)

let service_unavailable () =
  let result = with_test_resource begin fun resource ->
    resource#set_service_available false;
    Request.make ~meth:`HEAD Uri.(of_string "/")
  end in
  assert_path ~msg:"503 path" result Path.to_b13;
  assert_status ~msg:"503 result" result 503;
;;

(*
%% 503 result via B13 (at ping)
ping_invalid() ->
                                                % "breakout" for "anything other than pong"
    put_setting(ping, breakout),
    {ok, Result} = httpc:request(head, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 503, "Service Unavailable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B13,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 500 error response result via B13 (ping raises error)
ping_error() ->
    put_setting(ping, ping_raise_error),
    {ok, Result} = httpc:request(head, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 500, "Internal Server Error"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B13,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 500 error response via O18 from a callback raising an error
internal_server_error_o18() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain",
                                          size_stream_raises_error}]),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 500, "Internal Server Error"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% It seems that a callback that returns a callback is what's necessary to get
%% the top-level `catch` in webmachine_decision_core:handle_request to be
%% covered, hence the use of the range form of stream (other callbacks can also
%% cover the catch clause, but not without cluttering the test output with
%% badmatch errors from other modules)
size_stream_raises_error(ReqData, Context) ->
    Error = fun(_Start, _End) ->
                    error(foobar)
            end,
    {{stream, 1, Error}, ReqData, Context}.
*)

let not_implemented_b12 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods http_1_0_methods;
    resource#set_known_methods http_1_0_methods;
    Request.make ~meth:`DELETE Uri.(of_string "/")
  end in
  assert_path ~msg:"503 path" result Path.to_b12;
  assert_status ~msg:"501 result" result 501;
;;

let not_implemented_b6 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_valid_content_headers false;
    Request.make ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"501 path" result Path.to_b6;
  assert_status ~msg:"501 result" result 501;
;;

let uri_too_long_b11 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_uri_too_long true;
    Request.make ~meth:`GET Uri.(of_string "/toolong")
  end in
  assert_path ~msg:"414 path" result Path.to_b11;
  assert_status ~msg:"414 result via B11" result 414;
;;

let unsupported_media_type_b5 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_known_content_type false;
    Request.make ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"415 path" result Path.to_b5;
  assert_status ~msg:"415 result via B5" result 415;
;;

let request_entity_too_large_b4 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_valid_entity_length false;
    Request.make ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"413 path" result Path.to_b4;
  assert_status ~msg:"413 result via B4" result 413;
;;

let head_method_allowed () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET; `HEAD];
    Request.make ~meth:`HEAD Uri.(of_string "/foo")
  end in
  assert_path ~msg:"200 from head method allowed" result Path.to_o18_no_acpthead;
  assert_status ~msg:"200 from head method allowed" result 200;
;;

let head_method_not_allowed () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET; `POST; `PUT];
    Request.make ~meth:`HEAD Uri.(of_string "/foo")
  end in
  assert_path ~msg:"405 from head method not allowed" result Path.to_b10;
  assert_status ~msg:"405 from head method not allowed" result 405;
  assert_header ~msg:"405 from head method now allowed" result "allow" "GET,POST,PUT";
;;

let non_standard_method_501 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET; `POST; `PUT];
    Request.make ~meth:(`Other "FOO") Uri.(of_string "/foo")
  end in
  assert_status ~msg:"501 from non-standard method" result 501
;;

let non_standard_method_200 () =
  let method_ = "FOO" in
  let result = with_test_resource begin fun resource ->
    resource#set_known_methods (http_1_1_methods @ [`Other method_]);
    resource#set_allowed_methods [`GET; `POST; `PUT; `Other method_];
    Request.make ~meth:(`Other method_) Uri.(of_string "/foo")
  end in
  assert_status ~msg:"200 from non-standard method" result 200
;;

let bad_request_b9 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_malformed_request true;
    Request.make ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"400 result via B9" result Path.to_b9;
  assert_status ~msg:"400 result via B9" result 400;
;;

let simple_get () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_malformed_request false;
    Request.make ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"200 from a get" result Path.to_o18_no_acpthead;
  assert_status ~msg:"200 from a get" result 200;
;;

let not_acceptable_c4 () =
  let headers = Header.of_list [("Accept", "video/mp4")] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"406 result via C4" result Path.to_c4;
  assert_status ~msg:"406 result via C4" result 406;
;;

let not_acceptable_d5_c4 () =
  let headers = Header.of_list
    [("Accept", "text/plain"); ("Accept-Language", "x-pig-latin")]
  in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_content_types_provided [("text/plain", to_html)];
    resource#set_language_available false;
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"406 result via D5 via C4" result Path.to_d5_via_c4;
  assert_status ~msg:"406 result via D5 via C4" result 406;
;;

let not_acceptable_d5_c3 () =
  let headers = Header.of_list [("Accept-Language", "x-pig-latin")] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_content_types_provided [("text/plain", to_html)];
    resource#set_language_available false;
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"406 result via D5 via C3" result Path.to_d5_via_c3;
  assert_status ~msg:"406 result via D5 via C3" result 406;
;;

let not_acceptable_e6_d5_c3 () =
  let headers = Header.of_list
    [("Accept-Language", "en-us"); ("Accept-Charset", "iso-8859-1")]
  in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_content_types_provided [("text/plain", to_html)];
    resource#set_charsets_provided [("utf-8", fun x -> x)];
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"406 result via E6, D5, C3" result Path.to_e6_via_d5_c3;
  assert_status ~msg:"406 result via E6, D5, C3" result 406;
;;

let not_acceptable_f7_e6_d5_c4 () =
  let headers = Header.of_list
    [("Accept", "text/plain");
     ("Accept-Language", "en-us");
     ("Accept-Charset", "utf-8");
     ("Accept-Encoding", "gzip")]
  in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_content_types_provided [("text/plain", to_html)];
    resource#set_language_available true;
    resource#set_charsets_provided [("utf-8", fun x -> x)];
    resource#set_encodings_provided [];
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"406 result via D7, E6, D5, C4" result Path.to_f7_via_e6_d5_c4;
  assert_status ~msg:"406 result via D7, E6, D5, C4" result 406;
;;

let precond_fail_no_resource () =
  let headers = Header.of_list [("If-Match", "*")] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_resource_exists false;
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"412 result via H7" result Path.to_h7_no_acpthead;
  assert_status ~msg:"412 result via H7" result 412;
;;

let precond_fail_g11 () =
  let headers = Header.of_list [("If-Match", "\"v0\", \"v1\"")] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_generate_etag (Some "v2");
    Request.make ~headers ~meth:`GET Uri.(of_string "/")
  end in
  assert_path ~msg:"412 result via G11" result Path.to_g11_no_acpthead;
  assert_status ~msg:"412 result via G11" result 412;
;;

let precond_fail_h12 () =
  let ten_am = "Wed, 20 Feb 2013 10:00:00 GMT" in
  let five_pm = "Wed, 20 Feb 2013 17:00:00 GMT" in
  let headers = Header.of_list [("If-Unmodified-Since", ten_am)] in
  let result = with_test_resource' begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_last_modified (Some five_pm);
    Request.make ~headers ~meth:`GET Uri.(of_string "/"), `String "foo"
  end in
  assert_path ~msg:"412 result via h12, i13, i12, h10, h11" result Path.to_h12_no_acpthead;
  assert_status ~msg:"412 result via h12, i13, i12, h10, h11" result 412;
;;


let precond_fail_j18 () =
  let headers = Header.of_list [("If-None-Match", "*"); ("Content-Type", "text/plain")] in
  let result = with_test_resource' begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    Request.make ~headers ~meth:`PUT Uri.(of_string "/"), `String "foo"
  end in
  assert_path ~msg:"412 result via J18 via I13 via I12 via H10" result Path.to_j18_no_acpthead;
  (* TODO fix these ~msg strings*)
  assert_status ~msg:"412 result via J18 via I13 via I12 via H10" result 412;
;;

let precond_fail_j18_via_k13 () =
  let headers = Header.of_list [("If-Match", "v1");
                                ("If-None-Match", "v1");
                                ("If-Unmodified-Since", "nonsense-date");
                                ("Content-Type", "text/plain")] in
  let result = with_test_resource' begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_generate_etag (Some "v1");
    Request.make ~headers ~meth:`PUT Uri.(of_string "/"), `String "foo"
  end in
  assert_path ~msg:"412 result via J18 via K13 via H11 via G11" result Path.to_j18_no_acpthead_2;
  (* TODO fix these ~msg strings*)
  assert_status ~msg:"412 result via J18 via K13 via H11 via G11" result 412;
;;

let precond_fail_j18_via_h12 () =
  let ten_am =  "Wed, 20 Feb 2013 10:00:00 GMT" in
  let five_pm = "Wed, 20 Feb 2013 17:00:00 GMT" in
  let headers = Header.of_list [("If-Match", "*");
                                ("If-None-Match", "*");
                                ("If-Unmodified-Since", five_pm);
                                ("Content-Type", "text/plain")] in
  let result = with_test_resource' begin fun resource ->
    resource#set_last_modified (Some ten_am);
    resource#set_allowed_methods default_allowed_methods;
    Request.make ~headers ~meth:`PUT Uri.(of_string "/"), `String "foo"
  end in
  assert_path ~msg:"412 result via J18 via I13 via I12 via H12" result Path.to_j18_no_acpthead_3;
  assert_status ~msg:"412 result via J18 via I13 via I12 via H12" result 412;
;;

let content_valid () =
  let headers = Header.of_list [("Content-Type", "text/plain")] in
  let result = with_test_resource' begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_content_types_accepted [("text/plain", of_plain)];
    Request.make ~headers ~meth:`PUT Uri.(of_string "new"), `String "foo"
  end in
  let msg = "204 result via o20, p11, o14" in
  assert_path ~msg result Path.to_o20_via_p11_via_o14_no_acpthead;
  assert_status ~msg result 204
;;

(*
%% 204 result, content-md5 header matches
content_md5_valid_b9a() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    Body = "foo",
    MD5Sum = base64:encode_to_string(md5(Body)),
    Headers = [{"Content-MD5", MD5Sum}],
    PutRequest = {url("new"), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O20_VIA_P11_VIA_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 204 result, content-md5 header matches, but checked by
%% validate_content_checksum instead of webmachine_decision_core itself.
content_md5_valid_b9a_validated() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(validate_content_checksum, true),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    Body = "foo",
    MD5Sum = base64:encode_to_string(md5(Body)),
    Headers = [{"Content-MD5", MD5Sum}],
    PutRequest = {url("new"), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O20_VIA_P11_VIA_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 400 result, content-md5 header does not match
content_md5_invalid_b9a() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentType = "text/plain",
    Body = "foo",
    InvalidMD5Sum = base64:encode_to_string("this is invalid for foo"),
    Headers = [{"Content-MD5", InvalidMD5Sum}],
    PutRequest = {url(), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 400, "Bad Request"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B9,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 400 result, resource's custom validate_content_checksum function rejects it
content_md5_custom_inval_b9a() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(validate_content_checksum, false),
    ContentType = "text/plain",
    Body = "foo",
    InvalidMD5Sum = base64:encode_to_string("this is invalid for foo"),
    Headers = [{"Content-MD5", InvalidMD5Sum}],
    PutRequest = {url(), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 400, "Bad Request"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B9,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

*)
let authorized_b8 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_is_authorized (`Basic "basic");
    Request.make ~meth:`GET Uri.(of_string "/authorisedfoo")
  end in
  assert_path ~msg:"401 result via B9" result Path.to_b8;
  assert_status ~msg:"401 result via B8" result 401;
;;

let forbidden_b7 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_forbidden true;
    Request.make ~meth:`GET Uri.(of_string "/forbiddenfoo")
  end in
  assert_path ~msg:"403 result via B7" result Path.to_b7;
  assert_status ~msg:"403 result via B7" result 403;
;;


let options_b3 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET; `HEAD; `PUT; `OPTIONS];
    Request.make ~meth:`OPTIONS Uri.(of_string "/")
  end in
  assert_path ~msg:"200 result via OPTIONS" result Path.to_b3;
  assert_status ~msg:"200 result via OPTIONS" result 200;
;;

let variances_o18 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_charsets_provided [
      ("utf-8", fun x -> x);
      ("iso-8859-5", fun x -> x);
      ("unicode-1-1", fun x -> x);
    ];
    Request.make ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"200 result with Vary" result Path.to_o18_no_acpthead;
  assert_status ~msg:"200 result with Vary" result 200;
;;

let variances_o18_2 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_charsets_provided [("utf-8", fun x -> x)];
    resource#set_content_types_provided [
      ("text/html", to_html);
      ("text/plain", to_html);
    ];
    resource#set_encodings_provided [("identity", fun x -> x)];
    Request.make ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"200 result with other Vary" result Path.to_o18_no_acpthead;
  assert_status ~msg:"200 result with other Vary" result 200;
;;

(*
%% 200 result with body generation
ok_o18b() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(generate_etag, "v1"),
    put_setting(last_modified, ?FIRST_DAY_OF_LAST_YEAR),
    put_setting(expires, ?FIRST_DAY_OF_NEXT_YEAR),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.
*)

let multiple_choices_o18 () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET];
    resource#set_multiple_choices true;
    resource#set_charsets_provided [
      ("utf-8"      , fun x -> x);
      ("iso-8859-5" , fun x -> x);
      ("unicode-1-1", fun x -> x);
    ];
    Request.make ~meth:`GET Uri.(of_string "/foo");
  end in
  assert_path ~msg:"300 via o18" result Path.to_o18_no_acpthead;
  assert_status ~msg:"300 via o18" result 300;
;;

(*
%% 301 result via I4
moved_permanently_i4() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(moved_permanently, {true, url("new")}),
    PutRequest = {url("old"), [], "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 301, "Moved Permanently"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_I4_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 301 result via K5
moved_permanently_k5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    put_setting(moved_permanently, {true, url("new")}),
    %% We just want to get the 301 from httpc, we don't want it to actually
    %% try redirecting, so we turn off autoredirect
    HTTPOptions = [{autoredirect, false}],
    {ok, Result} = httpc:request(get, {url("old"), []}, HTTPOptions, []),
    ?assertMatch({{"HTTP/1.1", 301, "Moved Permanently"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_K5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 307 result via L5
moved_temporarily_l5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    put_setting(moved_temporarily, {true, url("new")}),
    %% We just want to get the 307 from httpc - similar to note about 301 above
    HTTPOptions = [{autoredirect, false}],
    {ok, Result}= httpc:request(get, {url("old"), []}, HTTPOptions, []),
    ?assertMatch({{"HTTP/1.1", 307, "Temporary Redirect"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_L5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.
*)

(* %% 304 result via J18 via K13 via H11 via G11 *)
let not_modified_j18 () =
  let headers = Header.of_list [("If-None-Match", "*")] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"304 path" result Path.to_j18_no_acpthead;
  assert_status ~msg:"304 result with if-none-match" result 304;
;;

(* %% 304 result via J18 via K13 via H11 via G11 *)
let not_modified_j18_via_k13 () =
  let headers = Header.of_list [
      ("If-Match", "\"v1\"");
      ("If-None-Match", "\"v1\"");
      ("If-Unmodified-Since", "{{INVALID DATE}}")
    ] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_generate_etag (Some "v1");
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"304 path" result Path.to_j18_no_acpthead_2;
  assert_status ~msg:"304 result with if-none-match" result 304;
;;

let not_modified_j18_multiple_if_match () =
  let headers = Header.of_list [
      ("If-Match", "\"v1\", \"v2\"");
      ("If-None-Match", "\"v1\", \"v2\""); (* this is required for GET/HEAD! *)
      ("If-Unmodified-Since", "{{INVALID DATE}}")
    ] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_generate_etag (Some "v2");
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"304 path" result Path.to_j18_no_acpthead_2;
  assert_status ~msg:"304 result with if-none-match" result 304;
;;

let not_modified_j18_multiple_if_none_match () =
  let headers = Header.of_list [
      ("If-None-Match", "\"v1\", \"v2\""); (* this is required for GET/HEAD! *)
    ] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_generate_etag (Some "v2");
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"304 path" result Path.to_j18_via_k13_i13_h10_g8_f6_e6_d5_c4;
  assert_status ~msg:"304 result with if-none-match" result 304;
;;

let not_modified_j18_multiple_if_none_match_first () =
  let headers = Header.of_list [
      ("If-None-Match", "\"v1\", \"v2\""); (* this is required for GET/HEAD! *)
    ] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_generate_etag (Some "v1");
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"304 path" result Path.to_j18_via_k13_i13_h10_g8_f6_e6_d5_c4;
  assert_status ~msg:"304 result with if-none-match" result 304;
;;

(* %% 304 result via J18 via I13 via I12 via H12 *)
let not_modified_j18_via_h12 () =
  let tenAM = "Wed, 20 Feb 2013 10:00:00 GMT"
  and fivePM = "Wed, 20 Feb 2013 17:00:00 GMT"
  in
  let headers = Header.of_list [
      ("If-Match", "*");
      ("If-None-Match", "*");
      ("If-Unmodified-Since", fivePM)
    ] in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_last_modified (Some tenAM);
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo")
  end in
  assert_path ~msg:"304 path" result Path.to_j18_no_acpthead_3;
  assert_status ~msg:"304 result with if-none-match" result 304;
;;

let not_modified_l17 () =
  let first_day_of_last_year = "Tue, 01 Jan 2015 00:00:00 GMT" in
  let first_day_of_next_year = "Sun, 01 Jan 2017 00:00:00 GMT" in
  let headers = Header.of_list [("If-Modified-Since", first_day_of_last_year)] in
  let result = with_test_resource begin fun resource ->
    resource#set_last_modified (Some first_day_of_last_year);
    resource#set_expires (Some first_day_of_next_year);
    Request.make ~headers ~meth:`GET Uri.(of_string "/foo");
  end in
  assert_path ~msg:"304 via l17" result Path.to_l17_no_acpthead;
  assert_status ~msg:"304 via l17" result 304;
;;

(*
%% 303 result via N11 using request data rewriting
see_other_n11() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(process_post, {set_resp_redirect, ?RESOURCE_PATH ++ "/new1"}),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [{autoredirect,false}], []),
    ?assertMatch({{"HTTP/1.1", 303, "See Other"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 500 result via N11 - Setting do_redirect without a Location
internal_server_error_n11() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(process_post, {set_resp_redirect_but_not_location}),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 500, "Internal Server Error"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 303 result via N11 using the result of resource calls
see_other_n11_resource_calls() ->
    see_other_n11_resource_calls_base_uri(undefined).

%% 303 result via N11 using the result of resource calls and a custom base_uri
see_other_n11_custom_base_uri() ->
    BaseURIFun = {decision_core_test, base_uri_add_slash},
    see_other_n11_resource_calls_base_uri(BaseURIFun).

%% 303 result via N11 using the result of resource calls and a passthrough
%% base_uri
see_other_n11_wrq_base_uri() ->
    BaseURIFun = {wrq, base_uri},
    see_other_n11_resource_calls_base_uri(BaseURIFun).

%% helper function to remove common code
see_other_n11_resource_calls_base_uri(Value) ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    put_setting(post_is_create, true),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(create_path, {set_resp_redirect, ?RESOURCE_PATH ++ "/new1"}),
    put_setting(base_uri, Value),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [{autoredirect,false}], []),
    ?assertMatch({{"HTTP/1.1", 303, "See Other"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.
*)

let created_n11_resource () =
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods [`GET; `POST; `PUT];
    resource#set_resource_exists true;
    resource#set_post_is_create true;
    resource#set_content_types_accepted ["text/html", of_plain];
    resource#set_create_path "new1";
    let headers = Header.of_list ["Content-Type", "text/html"] in
    Request.make ~headers ~meth:`POST Uri.(of_string "/foo")
  end in
  assert_status ~msg:"201 via n11" result 201;
  assert_header ~msg:"201 via n11 sets location header" result "Location" "/foo/new1";
;;

(*
%% 303 result via N5
see_other_n5() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    put_setting(allow_missing_post, true),
    put_setting(process_post, {set_resp_redirect, ?RESOURCE_PATH ++ "/new1"}),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [{autoredirect,false}], []),
    ?assertMatch({{"HTTP/1.1", 303, "See Other"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_N5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 404 result via L7
not_found_l7() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    {ok, Result} = httpc:request(get, {url("nothere"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 404, "Object Not Found"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_L7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 404 result via M7
not_found_m7() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 404, "Object Not Found"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 201 result via P11 from POST
created_p11_post() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    put_setting(process_post, {new_resource, ?RESOURCE_PATH ++ "/new1"}),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.
*)

let create_p11_put () =
  let headers = Header.init_with "Content-Type" "text/plain" in
  let result = with_test_resource begin fun resource ->
    resource#set_allowed_methods default_allowed_methods;
    resource#set_resource_exists false;
    resource#set_content_types_accepted [
      "text/plain", of_plain_with_loc "new"
    ];
    resource#set_is_conflict false;
    Request.make ~headers ~meth:`PUT Uri.(of_string "/put")
  end in
  assert_path ~msg:"201 via P11" result Path.to_p11_via_p3_no_acpthead;
  assert_status ~msg:"201 via P11" result 201

(*
%% 409 result via P3 (must be a PUT)
conflict_p3() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(is_conflict, true),
    PutRequest = {url("put"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 409, "Conflict"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P3_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 409 result via O14
conflict_o14() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(is_conflict, true),
    PutRequest = {url("put"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 409, "Conflict"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 410 result via M5
gone_m5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    {ok, Result} = httpc:request(get, {url("gone"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 410, "Gone"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_M5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 410 result via N5
gone_n5() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 410, "Gone"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 202 result via M20 - The delete has been "accepted" but it didn't actually
%% happen (or, rather, may or may not happen in the future)
accepted_m20() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS ++ ['DELETE']),
    put_setting(delete_resource, true),
    put_setting(delete_completed, false),
    DeleteRequest = {url("doomed"), []},
    {ok, Result} = httpc:request(delete, DeleteRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 202, "Accepted"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_M20_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 415 via accept_helper - This path is not explicit in the state diagram in
%% http-headers-status-v3.png, but is a path in the
%% webmachine_decision_core.erl logic. The accept_helper function itself can be
%% reached from P3 (as a PUT), O14 (as a PUT), or N11 (as a POST).
unsupported_media_type_accept_helper() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    HTMLContent = "text/html",
    PlainTextContent = "text/plain",
    put_setting(content_types_accepted, [{HTMLContent, to_html}]),
    put_setting(is_conflict, false),
    PutRequest = {url("put"), [], PlainTextContent, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 415, "Unsupported Media Type"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 201 result via P11 with body streaming
created_p11_streamed() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    NewLocation = ?RESOURCE_PATH ++ "/posted",
    put_setting(process_post,
                {mfa, ?MODULE, process_post_for_created_p11, NewLocation}),
    ContentType = "text/plain",
    FooPrime = string:copies("foo", 128),
    PostRequest = {url("post"), [], ContentType, FooPrime},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

process_post_for_created_p11(ReqData, Context, NewLocation) ->
    StreamBody = wrq:stream_req_body(ReqData, 3),
    Body = get_streamed_body(StreamBody, []),
    StreamedResponse = send_streamed_body(Body, 4),
    RDWithBody = wrq:set_resp_body({stream, StreamedResponse}, ReqData),
    Headers = [{"Location", NewLocation}],
    RDWithBodyAndLocation = wrq:set_resp_headers(Headers, RDWithBody),
    {true, RDWithBodyAndLocation, Context}.

%% The get_streamed_body and send_streamed_body functions here are derived from
%% the example in the Webmachine docs
get_streamed_body({Hunk, done}, Acc) ->
    List = lists:reverse([Hunk | Acc]),
    iolist_to_binary(List);
get_streamed_body({Hunk, Next}, Acc) ->
    get_streamed_body(Next(), [Hunk | Acc]).

send_streamed_body(Body, Max) ->
    HunkLen = 8 * Max,
    case Body of
        <<Hunk:HunkLen/bitstring, Rest/binary>> ->
            {Hunk, fun() -> send_streamed_body(Rest, Max) end};
        _ ->
            {Body, done}
    end.

%% 201 result via P11, exercising webmachine_decision_core:accept_helper code
created_p11_accept_helper() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    FourtyTwoMugs = string:copies("mug", 42),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, accept_text}]),
    put_setting(is_conflict, {new_location, url("new")}),
    PutRequest = {url("put"), [], ContentType, FourtyTwoMugs},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_P3_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

accept_text(ReqData, Context) ->
    ReqBody = wrq:req_body(ReqData),
    Text = binary:bin_to_list(ReqBody),
    Reply = binary:list_to_bin("Recieved: " ++ Text ++ "."),
    RDWithBody = wrq:set_resp_body(Reply, ReqData),
    {true, RDWithBody, Context}.

%% 200 result from a GET using the "Write callable response method," which is
%% commented on this commit:
%% github.com/basho/webmachine/commit/96f5c5a679595e3554fc3e6af565faf5c6e37bbd
writer_callback() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", writer_response}]),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

writer_response(ReqData, Context) ->
    Body =
        fun(Write) ->
                Content = string:copies("mug", 42),
                Write(Content)
        end,
    {{writer, Body}, ReqData, Context}.

%% 200 result from a HEAD when the length is known, a special case for Riak CS
head_length_access_for_cs() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", known_length_body}]),
    {ok, Result} = httpc:request(head, {url("knownlength"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result from a GET when the length is known, a special case for Riak CS
get_known_length_for_cs() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", known_length_body}]),
    {ok, Result} = httpc:request(get, {url("knownlength"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

known_length_body(ReqData, Context) ->
    Content = "You have requested " ++ wrq:raw_path(ReqData) ++ ".",
    Size = string:len(Content),
    StreamBody = send_streamed_body(Content, 4),
    {{known_length_stream, Size, StreamBody}, ReqData, Context}.

%% 200 result from a GET exercising the range response form of returning bodies
get_for_range_capable_stream() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", range_response}]),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% Return a function that, when given a range, returns a StreamBody for the
%% content in that range
range_response(ReqData, Context) ->
    Content = string:copies("mug", 42),
    Size = string:len(Content),
    Fun = fun(Start, End) ->
                  Length = (End - Start) + 1,
                  Result = string:substr(Content, Start + 1, Length),
                  {Result, done}
          end,
    {{stream, Size, Fun}, ReqData, Context}.

%% 201 result via P11 from a POST with a streaming/chunked body and an MD5-sum
%%
%% This test exposes a bug with the code that checks a stream's MD5 sum
%% Known Failure
stream_content_md5() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'POST', 'PUT']),
    put_setting(validate_content_checksum,
                {mfa, ?MODULE, validate_checksum_for_md5stream, not_validated}),
    NewLocation = ?RESOURCE_PATH ++ "/posted",
    put_setting(process_post,
                {mfa, ?MODULE, process_post_for_md5_stream, NewLocation}),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    ContentType = "text/plain",
    Content = "foo",
    ValidMD5Sum = base64:encode_to_string(md5(Content)),
    ibrowse:start(),
    Url = url("post"),
    Headers = [{"Content-Type", ContentType},
               {"Content-MD5", ValidMD5Sum},
               {"Expect", "100-continue"}],
    BodyGenerator = fun(Step) ->
                            case Step of
                                0 -> {ok, Content, Step + 1};
                                _ -> eof
                            end
                    end,
    Body = {BodyGenerator, 0},
    Options = [{transfer_encoding, {chunked, 3}}],
    Result = ibrowse:send_req(Url, Headers, post, Body, Options),
    {ok, Status, _RespHeaders, _RespBody} = Result,
    ?assertEqual("201", Status),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

validate_checksum_for_md5stream(ReqData, Context, Result) ->
    _StreamBody = wrq:stream_req_body(ReqData, 5),
    {Result, ReqData, Context}.

process_post_for_md5_stream(ReqData, Context, NewLocation) ->
    Headers = [{"Location", NewLocation}],
    RDWithLocation = wrq:set_resp_headers(Headers, ReqData),
    %% ReqBody = wrq:stream_req_body(ReqData, 1024),
    %% Text = get_streamed_body(ReqBody, []),
    %% Text = wrq:req_body(ReqData),
    {true, RDWithLocation, Context}.
*)

let rec was_successful =
  function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
        was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
        false

let _ =
  let tests = [
    "service_unavailable" >:: service_unavailable;
    "not implemented (b12)" >:: not_implemented_b12;
    "not implemented (b6)" >:: not_implemented_b6;
    "uri too long (b11)" >:: uri_too_long_b11;
    "unsupported media type (b5)" >:: unsupported_media_type_b5;
    "request entity too large (b4)" >:: request_entity_too_large_b4;
    "head method allowed" >:: head_method_allowed;
    "head method not allowed" >:: head_method_not_allowed;
    "non-standard method 501" >:: non_standard_method_501;
    "non-standard method 200" >:: non_standard_method_200;
    "bad request (b9)" >:: bad_request_b9;
    "simple get" >:: simple_get;
    "not_acceptable_c4" >:: not_acceptable_c4;
    "not_acceptable_d5_c4" >:: not_acceptable_d5_c4;
    "not_acceptable_d5_c3" >:: not_acceptable_d5_c3;
    "not_acceptable_e6_d5_c3" >:: not_acceptable_e6_d5_c3;
    "not_acceptable_f7_e6_d5_c4" >:: not_acceptable_f7_e6_d5_c4;
    "precond_fail_no_resource" >:: precond_fail_no_resource;
    "precond_fail_g11" >:: precond_fail_g11;
    "precond_fail_h12" >:: precond_fail_h12;
    "precond_fail_j18" >:: precond_fail_j18;
    "precond_fail_j18_via_k13" >:: precond_fail_j18_via_k13;
    "precond_fail_j18_via_h12" >:: precond_fail_j18_via_h12;
    "content_valid" >:: content_valid;
    "authorized_b8" >:: authorized_b8;
    "forbidden_b7" >:: forbidden_b7;
    "options_b3" >:: options_b3;
    "variances_o18" >:: variances_o18;
    "variances_o18_2" >:: variances_o18_2;
    "multiple_choices_o18" >:: multiple_choices_o18;
    "not_modified_l17" >:: not_modified_l17;
    "not_modified_j18" >:: not_modified_j18;
    "not modified j18 via k13" >:: not_modified_j18_via_k13;
    "not modified j18 multiple if-match" >:: not_modified_j18_multiple_if_match;
    "not modified j18 multiple if-none-match first" >:: not_modified_j18_multiple_if_none_match_first;
    "not modified j18 multiple if-none-match second" >:: not_modified_j18_multiple_if_none_match;
    "not modified j18 via h12" >:: not_modified_j18_via_h12;
    "create_p11_put" >:: create_p11_put;
    "created_n11_resource" >:: created_n11_resource;
  ] in
  let suite = (Printf.sprintf "test logic") >::: tests in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite))
  then exit 1
