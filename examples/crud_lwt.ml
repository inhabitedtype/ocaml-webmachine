(** An example CRUD API for id-based operations on JSON objects. The objects are
    stored in-memory and therefore will not persist across runs of the database.
    The application does not perform any JSON validation at this time.

    Build by running `dune`:

      [dune build examples/crud_lwt.exe]

    Run using the following command, which will display the path that each
    request takes through the decision diagram:

      [DEBUG_PATH= ./crud_lwt.exe]

    Here are some sample CURL commands to test on a running server:

      - Get a complete list of items:
        [curl -i -w "\n" -X GET http://localhost:8080/items]

      - Get the item with id 1:
        [curl -i -w "\n" -X GET http://localhost:8080/item/1]

      - Create a new item:
        [curl -i -w "\n" -X POST -d '{"name":"new item"}' http://localhost:8080/items]

      - Modify the item with id 1:
        [curl -i -w "\n" -X PUT -H 'Content-Type: application/json'\
          -d '{"name":"modified item"}' http://localhost:8080/item/1]

      - Delete the item with id 1:
        [curl -i -w "\n" -X DELETE http://localhost:8080/item/1] *)

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
        (Some(id, List.assoc id l), l)
      else
        (None, l))

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
          then begin found := true; (id', e) end
          else (id', e'))
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
  module UnixClock = struct
    let now = fun () -> int_of_float (Unix.gettimeofday ())
  end
  include Webmachine.Make(Cohttp_lwt_unix__Io)(UnixClock)
end

(** A resource for querying all the items in the database via GET and creating
    a new item via POST. Check the [Location] header of a successful POST
    response for the URI of the item. *)
class items db = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private to_json rd =
    Db.get_all db
    >|= List.map snd
    >>= fun values ->
      let json = Printf.sprintf "[%s]" (String.concat ", " values) in
      Wm.continue (`String json) rd

  method! allowed_methods rd =
    Wm.continue [`GET; `HEAD; `POST] rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method! process_post rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Db.add db body >>= fun new_id ->
    let rd' = Wm.Rd.redirect ("/item/" ^ (string_of_int new_id)) rd in
    Wm.continue true rd'
end

(** A resource for querying an individual item in the database by id via GET,
    modifying an item via PUT, and deleting an item via DELETE. *)
class item db = object(self)
  inherit [Cohttp_lwt.Body.t] Wm.resource

  method private of_json rd =
    Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body >>= fun body ->
    Db.put db (self#id rd) body >>= fun modified ->
      let resp_body =
        if modified
          then `String "{\"status\":\"ok\"}"
          else `String "{\"status\":\"not found\"}"
      in
      Wm.continue modified { rd with Wm.Rd.resp_body }

  method private to_json rd =
    Db.get db (self#id rd)
    >>= function
      | None            -> assert false
      | Some (_, value) ->
        Wm.continue (`String value) rd

  method! allowed_methods rd =
    Wm.continue [`GET; `HEAD; `PUT; `DELETE] rd

  method! resource_exists rd =
    Db.get db (self#id rd)
    >>= function
      | None   -> Wm.continue false rd
      | Some _ -> Wm.continue true rd

  method content_types_provided rd =
    Wm.continue [
      "application/json", self#to_json
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "application/json", self#of_json
    ] rd

  method! delete_resource rd =
    Db.delete db (self#id rd)
    >>= fun deleted ->
      let resp_body =
        if deleted
          then `String "{\"status\":\"ok\"}"
          else `String "{\"status\":\"not found\"}"
      in
      Wm.continue deleted { rd with Wm.Rd.resp_body }

  method private id rd =
    int_of_string (Wm.Rd.lookup_path_info_exn "id" rd)
end

let main () =
  (* listen on port 8080 *)
  let port = 8080 in
  (* create the database *)
  let db = Db.create () in
  (* the route table *)
  let routes = [
    ("/items", fun () -> new items db) ;
    ("/item/:id", fun () -> new item db) ;
  ] in
  let callback (_ch, _conn) request body =
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
       *  [$ DEBUG_PATH= ./crud_lwt.native]
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
  let conn_closed (ch, _conn) =
    Printf.printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  (* init the database with two items *)
  Db.add db "{\"name\":\"item 1\"}" >>= fun _ ->
  Db.add db "{\"name\":\"item 2\"}" >>= fun _ ->
  let config = Server.make ~callback ~conn_closed () in
  Server.create  ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port;
      Lwt.return_unit)

let () =  Lwt_main.run (main ())
