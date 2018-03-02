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

class base_path str = object
  (* A test resource that accepts GET requests and returns the string provided
   * to the constructor *)

  inherit [Cohttp.Body.t] Webmachine.resource

  method content_types_provided rd =
    Webmachine.continue [
      ("application/text", Webmachine.continue (`String str))
    ] rd

  method content_types_accepted rd =
    Webmachine.continue [] rd

  method allowed_methods rd =
    Webmachine.continue [`GET] rd
end

class param_path key = object
  (* A test resource that accepts GET requests and returns the path info
   * corresponing to the key provided to the constructor. *)

  inherit [Cohttp.Body.t] Webmachine.resource

  method content_types_provided rd =
    Webmachine.continue [
      ("application/text", Webmachine.(continue (`String (Rd.lookup_path_info_exn key rd))))
    ] rd

  method content_types_accepted rd =
    Webmachine.continue [] rd

  method allowed_methods rd =
    Webmachine.continue [`GET] rd
end

class disp_path = object
  (* A test resource that accepts GET requests and returns the disp_path that
   * corresponds to the part of the path matched by the wildcard pattern. *)
  inherit [Cohttp.Body.t] Webmachine.resource

  method content_types_provided rd =
    Webmachine.continue [
      ("application/text", Webmachine.(continue (`String rd.Rd.dispatch_path)))
    ] rd

  method content_types_accepted rd =
    Webmachine.continue [] rd

  method allowed_methods rd =
    Webmachine.continue [`GET] rd
end


open OUnit

let with_path path table =
  let open Cohttp in
  let uri = Uri.make ~scheme:"http" ~host:"localhost" ~path () in
  let headers = Header.init_with "accept" "application/text" in
  let request = Request.make ~meth:`GET ~headers uri in
  Webmachine.dispatch' table ~body:`Empty ~request

let empty () =
  "an empty table will produce no result"
    @? begin match run (with_path "/" []) with
       | None -> true
       | _    -> false
    end

let single () =
  let table = ["/", fun () -> new base_path "root"] in
  "a single entry for the root will dispatch the root to it"
    @? begin match run (with_path "/" table) with
       | Some(`OK, _, `String "root", _) -> true
       | _                               -> false
    end;
  "a single entry for the root will not dispatch anything else to it"
    @? begin match run (with_path "/foo" table) with
       | None -> true
       | _    -> false
    end

let overlap () =
  let table =
    [ ("/foo"    , fun () -> new base_path "/foo")
    ; ("/foo/bar", fun () -> new base_path "/foo/bar")
    ; ("/foo/baz", fun () -> new base_path "/foo/baz")
    ; ("/bar/baz", fun () -> new base_path "/bar/baz")
    ; ("/bar/foo", fun () -> new base_path "/bar/foo")
    ; ("/bar"    , fun () -> new base_path "/bar")
    ]
  in
  "a leading prefix pattern gets matched"
    @? begin match run (with_path "/foo" table) with
       | Some(`OK, _, `String "/foo", _) -> true
       | _                               -> false
    end;
  "a trailing prefix pattern gets matched"
    @? begin match run (with_path "/bar" table) with
       | Some(`OK, _, `String "/bar", _) -> true
       | _                               -> false
    end;
  "a complete pattern does not get shadowed by prefix"
    @? begin match run (with_path "/foo/baz" table) with
       | Some(`OK, _, `String "/foo/baz", _) -> true
       | _                                   -> false
    end

let keys () =
  let table =
    [ ("/foo/:id"         , fun () -> new param_path "id")
    ; ("/foo/:id/:bar"    , fun () -> new param_path "bar")
    ; ("/foo/:id/bar/:baz", fun () -> new param_path "baz")
    ]
  in
  "a leading prefix pattern gets matched and keys properly assigned"
    @? begin match run (with_path "/foo/1" table) with
       | Some(`OK, _, `String "1", _) -> true
       | _                            -> false
    end;
  "a pattern with keys does not get shadowed by prefix"
    @? begin match run (with_path "/foo/1/test" table) with
       | Some(`OK, _, `String "test", _) -> true
       | _                               -> false
    end;
  "a patternw with interleaved keys and literals works"
    @? begin match run (with_path "/foo/1/bar/test" table) with
       | Some(`OK, _, `String "test", _) -> true
       | _                               -> false
    end

let wildcard () =
  let table = ["/foo/*", fun () -> new disp_path] in
  "a trailing wildcard pattern matches just the prefix"
    @? begin match run (with_path "/foo" table) with
       | Some(`OK, _, `String "", _) -> true
       | _                           -> false
    end;
  "a trailing wildcard pattern matches a longer path"
    @? begin match run (with_path "/foo/bar/baz" table) with
       | Some(`OK, _, `String "bar/baz", _) -> true
       | _                               -> false
    end

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
    "empty" >:: empty;
    "single" >:: single;
    "overlap" >:: overlap;
    "keys" >:: keys;
    "wildcard" >:: wildcard
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
