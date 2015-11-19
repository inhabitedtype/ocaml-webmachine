(*
 * CRUD (Create, Retrieve, Update, Delete) API example.
 * The application basically stores the request body
 * data in an association list. There is no real JSON
 * decoding.
 *
 * - How to build:
 *   ocamlbuild -use-ocamlfind -pkgs lwt,cohttp.lwt,webmachine
 *     crud_lwt.native
 *
 * - Run the server:
 *   ./crud_lwt.native
 *
 * - Test with curl (http://curl.haxx.se/):
 *
 *   - Get a complete list of items:
 *     curl -i -w "\n" -X GET http://127.0.0.1:8080/items
 *
 *   - Get the item with id 1:
 *     curl -i -w "\n" -X GET http://127.0.0.1:8080/items/1
 *
 *   - Create a new item:
 *     curl -i -w "\n" -X POST -d '{"name":"new item"}'
 *       http://127.0.0.1:8080/items
 *
 *   - Modify the item with id 1:
 *     curl -i -w "\n" -X PUT -H 'Content-Type: application/json'
 *       -d '{"name":"modified item"}' http://127.0.0.1:8080/items/1
 *
 *   - Delete the item with id 1:
 *     curl -i -w "\n" -X DELETE http://127.0.0.1:8080/items/1
*)

open Cohttp_lwt_unix
open Lwt.Infix

(* Toy database to store the items *)
module Db = struct
  let create () =
    Lwt_mvar.create []

  let id = ref 0

  let with_db db ~f =
    Lwt_mvar.take db   >>= fun l ->
      let result, l' = f l in
      Lwt_mvar.put db l' >|= fun () ->
        result

  let get db id =
    with_db db ~f:(fun l ->
      if (List.mem_assoc id l) then
        ([(id, List.assoc id l)], l)
      else
        ([], l))

  let get_all db =
    with_db db ~f:(fun l -> (l, l))

  let add db e =
    with_db db ~f:(fun l ->
      incr id;
      let l' = List.merge (fun x y -> compare (fst x) (fst y)) [!id, e] l in
      (!id, l'))

  let put db id e =
    let found = ref false in
    with_db db ~f:(fun l ->
      let l' = List.map (fun (id', e') ->
        if id' = id
          then begin found := true; (id', e') end
          else (id', e))
        l
      in
      (!found, l'))

  let delete db id =
    let deleted = ref false in
    with_db db ~f:(fun l ->
      let l' = List.filter (fun (id', _) ->
        deleted := !deleted || id' <> id;
        id <> id) l
      in
      (!deleted, l'))
end

(* Apply the [Webmachine.Make] functor to the Lwt_unix-based IO module
 * exported by cohttp. For added convenience, include the [Rd] module
 * as well so you don't have to go reaching into multiple modules to
 * access request-related information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end

(* Create a new class that inherits from [Wm.resource] and provides
 * implementations for its two virtual methods, and overrides some of
 * its default methods. *)
class item db = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  (* GET: retrieve an item or a list of items
   * POST: create a new item
   * PUT: modify an item
   * DELETE: delete an item *)
  method allowed_methods rd =
    Wm.continue [`GET; `POST; `PUT; `DELETE] rd

  (* Always return JSON *)
  method content_types_provided rd =
    (* self#retrieve_item will be called for GET request *)
    Wm.continue [
      ("application/json", self#retrieve_item);
    ] rd

  (* Accept only JSON data for PUT request *)
  method content_types_accepted rd =
    (* self#put will be called for PUT request *)
    Wm.continue [
      ("application/json", self#put);
    ] rd

  (* Method called for POST request *)
  method process_post rd =
    self#create_item rd
    >>= Wm.continue true

  (* Method called for DELETE request *)
  method delete_resource rd =
    self#delete_item rd
    >>= Wm.continue true

  (* Get the id parameter from the url *)
  method private id rd =
    try
      let s = Wm.Rd.lookup_path_info_exn "id" rd in
      int_of_string s
    with
    | _ -> 0

  (* PUT request *)
  method private put rd =
    self#modify_item rd >>= Wm.continue true

  (* Create a new item *)
  method private create_item rd =
    (* get the request body (should contains JSON) *)
    Cohttp_lwt_body.to_string rd.Wm.Rd.req_body >>= fun v ->
    Db.add db v >>= fun new_id ->
      (* return JSON data *)
      let json = Printf.sprintf "{\"id\":%d}" new_id in
      (* return also the location of this new item *)
      let h = rd.Wm.Rd.resp_headers in
      let h = Cohttp.Header.add_unless_exists h "location"
          ("/items/" ^ (string_of_int new_id))
      in
      Lwt.return { rd with Wm.Rd.resp_body = `String json;
                           resp_headers = h }

  (* Modify an item *)
  method private modify_item rd =
    (* get the request body (should contains JSON) *)
    Cohttp_lwt_body.to_string rd.Wm.Rd.req_body >>= fun v ->
    (* get the item id from the url *)
    let id = self#id rd in
    (* modify the item *)
    Db.put db id v >>= fun modified ->
      (* return JSON data *)
      let json =
        if modified then
          "{\"status\":\"not found\"}"
        else
          "{\"status\":\"ok\"}"
      in
      Lwt.return { rd with Wm.Rd.resp_body = `String json }

  (* Delete an item *)
  method private delete_item rd =
    (* get the item id from the url *)
    let id = self#id rd in
    (* delete the item *)
    Db.delete db id >>= fun deleted ->
      (* return JSON data *)
      let json =
        if deleted then
          "{\"status\":\"not found\"}"
        else
          "{\"status\":\"ok\"}"
      in
      Lwt.return { rd with Wm.Rd.resp_body = `String json }

  (* Get item(s) in JSON *)
  method private retrieve_item rd =
    (* if there is no id, get all the items *)
    let id = self#id rd in
    let items =
      if (id = 0) then
        Db.get_all db
      else
        Db.get db id
    in
    items >>= fun items ->
      let values = List.map snd items in
      let json = Printf.sprintf "[%s]" (String.concat ", " values) in
      Wm.continue (`String json) rd
end

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the database *)
  let db = Db.create () in
  (* init the database with two items *)
  let _ =
    Db.add db "{\"name\":\"item 1\"}" >>=
    (fun _ -> Db.add db "{\"name\":\"item 2\"}")
  in
  (* the route table *)
  let routes = [
    ("/items", fun () -> new item db) ;
    ("/items/:id", fun () -> new item db) ;
  ] in
  let callback (ch,conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Wm.dispatch' routes ~body ~request
    >|= begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status, headers, body, path) ->
      (* If you'd like to see the path that the request took through the
       * decision diagram, then run this example with the [DEBUG_PATH]
       * environment variable set. This should suffice:
       *
       *  [$ DEBUG_PATH= ./hello_lwt.native]
       *
       *)
      let path =
        match Sys.getenv "DEBUG_PATH" with
        | _ -> Printf.sprintf " - %s" (String.concat ", " path)
        | exception Not_found   -> ""
      in
      Printf.eprintf "%d - %s %s%s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        path;
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* create the server and handle requests with the function defined above *)
  let conn_closed (ch,conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
