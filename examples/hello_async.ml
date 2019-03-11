open Core
open Async

open Cohttp_async

(* Apply the [Webmachine.Make] functor to the Async-based IO module exported by
 * cohttp. For added convenience, include the [Rd] module as well so you don't
 * have to go reaching into multiple modules to access request-related
 * information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_async.Io)(UnixClock)
end

(* Create a new class that inherits from [Wm.resource] and provides
 * implementations for its two virtual methods, and overrides some of its default methods.
 *)
class hello = object(self)
  inherit [Body.t] Wm.resource

  (* Only allow GET requests to this resource *)
  method! allowed_methods rd =
    Wm.continue [`GET] rd

  (* Setup the resource to handle multiple content-types. Webmachine will
   * perform content negotiation as described in RFC 7231:
   *
   *   https://tools.ietf.org/html/rfc7231#section-5.3.2
   *
   * Content negotiation can be a complex process. Hoever for simple Accept
   * headers its fairly straightforward. Here's what content negotiation will
   * produce in some of these simple cases:
   *
   *     Accept             | Called method
   *   ---------------------+----------------
   *     "text/plain"       | self#to_text
   *     "text/html"        | self#to_html
   *     "text/*"           | self#to_html
   *     "application/json" | self#to_json
   *     "application/*"    | self#to_json
   *     "*/*"              | self#to_html
   *)
  method content_types_provided rd =
    Wm.continue [
      ("text/html"       , self#to_html);
      ("text/plain"      , self#to_text);
      ("application/json", self#to_json);
    ] rd

  (* Since only GET requests are allowed, there's no need to provide handlers
   * for requests containing certain content types. This method will never be
   * called, but it's necessary to provide an implementation since it's
   * [virtual] in the [Wm.resource] virtual class. *)
  method content_types_accepted rd =
    Wm.continue [] rd

  (* A helper method that returns what to say hello to. If a path wildcard
   * called ["what"] was introduced by a route, it will use whatever string in
   * that position. Otherwise, it defaults to the string ["world"]. *)
  method private what rd =
    try Wm.Rd.lookup_path_info_exn "what" rd with Caml.Not_found -> "world"

  (* Returns an html-based representation of the resource *)
  method private to_html rd =
    let body =
      Printf.sprintf
        "<html><body><h1>Hello, %s!</h1></body></html>"
        (self#what rd)
    in
    Wm.continue (`String body) rd

  (* Returns a plaintext representation of the resource *)
  method private to_text rd =
    let text= Printf.sprintf "Hello, %s!" (self#what rd) in
    Wm.continue (`String text) rd

  (* Returns a json representation of the resource *)
  method private to_json rd =
    let json =
      Printf.sprintf "{\"message\" : \"Hello, %s!\"}" (self#what rd)
    in
    Wm.continue (`String json) rd
end

let main () =
  (* Listen on port 8080 *)
  let port = 8080 in
  (* The route table. Both routes use the [hello] resource defined above.
   * However, the second one containes the [:what] wildcard in the path. The
   * value of that wildcard can be accessed in the resource by calling
   *
   *   [Wm.Rd.lookup_path_info "what" rd]
   *)
  let routes = [
    ("/"           , fun () -> new hello);
    ("/hello/:what", fun () -> new hello);
  ] in
  let handler ~body _ request =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Wm.dispatch' routes ~body ~request
    >>| begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status_code, headers, body, path) ->
      (* If you'd like to see the path that the request took through the
       * decision diagram, then run this example with the [DEBUG_PATH]
       * environment variable set. This should suffice:
       *
       *  [$ DEBUG_PATH= ./hello_async.native]
       *
       *)
      let path =
        match Sys.getenv "DEBUG_PATH" with
        | None   -> ""
        | Some _ -> sprintf " - %s" (String.concat ~sep:", " path)
      in
      Log.Global.info "%d - %s %s%s"
          (Code.code_of_status status_code)
          (Code.string_of_method (Request.meth request))
          (Uri.path (Request.uri request))
          path;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body status_code
  in
  (* Create the server and handle requests with the function defined above. Try
   * it out with some of these curl commands:
   *
   *   [curl -H"Accept:text/html" "http://localhost:8080"]
   *   [curl -H"Accept:text/plain" "http://localhost:8080"]
   *   [curl -H"Accept:application/json" "http://localhost:8080"]
   *)
  Server.create ~on_handler_error:`Raise (Tcp.Where_to_listen.of_port port) handler
  >>> (fun _server ->
    Log.Global.info "hello_async: listening on 0.0.0.0:%d%!" port)
;;

Scheduler.go_main ~main ()
