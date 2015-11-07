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

  let get db id =
    Lwt_mvar.take db >>= (fun l ->
        let _ = Lwt_mvar.put db l in
        let e = 
          if (List.mem_assoc id l) then
            [(id, List.assoc id l)]
          else
            []
        in        
        Lwt.return e
      )

  let get_all db =
    Lwt_mvar.take db >>= (fun l ->
        let _ = Lwt_mvar.put db l in
        let l = List.fast_sort (fun (id1, _) (id2, _) -> compare id1 id2) l in
        Lwt.return l
      )

  let add db e =
    Lwt_mvar.take db >>= (fun l ->
        let l = List.fast_sort (fun (id1, _) (id2, _) -> compare id2 id1) l in
        let id = 
          match l with
          | [] -> 1
          | (max, _) :: _ ->max + 1
        in
        let l = (id, e) :: l in
        let _ = Lwt_mvar.put db l in
        Lwt.return id
      )

  let put db id e =
    Lwt_mvar.take db >>= (fun l ->
        let id = 
          if (List.mem_assoc id l) then
            let l = List.remove_assoc id l in
            let l = (id, e) :: l in
            let _ = Lwt_mvar.put db l in
            id
          else
            let _ = Lwt_mvar.put db l in
            0
        in
        Lwt.return id
      )

  let delete db id =
    Lwt_mvar.take db >>= (fun l ->
        let id = 
          if (List.mem_assoc id l) then
            let l = List.remove_assoc id l in
            let _ = Lwt_mvar.put db l in
            id
          else
            let _ = Lwt_mvar.put db l in
            0
        in
        Lwt.return id
      )

end

(* Log to stderr *)
module Log = struct
  let method_called s = 
    Printf.eprintf "Method called: %s\n" s ;
    flush_all ()

  let info s = 
    Printf.eprintf "%s\n" s ;
    flush_all ()

  let error s = 
    Printf.eprintf "Error: %s\n" s ;
    flush_all ()
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

  val db = db

  method private get_db = db

  (* GET: retrieve an item or a list of items 
   * POST: create a new item
   * PUT: modify an item
   * DELETE: delete an item *)
  method allowed_methods rd =
    Log.method_called "allowed_methods" ;
    Wm.continue [`GET; `POST; `PUT; `DELETE] rd

  (* Always return JSON *)
  method content_types_provided rd =
    Log.method_called "content_types_provided" ;
    (* self#retrieve_item will be called for GET request *)
    Wm.continue [
      ("application/json", self#retrieve_item);
    ] rd

  (* Accept only JSON data for PUT request *)
  method content_types_accepted rd =
    Log.method_called "content_types_accepted" ;
    (* self#put will be called for PUT request *)
    Wm.continue [
      ("application/json", self#put);
    ] rd

  (* Method called for POST request *)
  method process_post rd =
    Log.method_called "process_post" ;
    self#create_item rd
    >>= Wm.continue true

  (* Method called for DELETE request *)
  method delete_resource rd =
    Log.method_called "delete_resource" ;
    self#delete_item rd
    >>= Wm.continue true

  (* Get the id parameter from the url *)
  method private id rd =
    Log.method_called "id" ;
    try 
      let s = Wm.Rd.lookup_path_info_exn "id" rd in
      int_of_string s
    with
    | _ -> 0

  (* PUT request *)
  method private put rd =
    Log.method_called "put" ;
    self#modify_item rd >>= Wm.continue true

  (* Create a new item *)
  method private create_item rd = 
    Log.method_called "create_item" ;
    (* get the request body (should contains JSON) *)
    Cohttp_lwt_body.to_string rd.Wm.Rd.req_body >>= (
      fun v -> 
        Log.info (Printf.sprintf "Body content: %s" v) ;
        (* create a new item *)
        Db.add self#get_db v >>= (
          fun new_id ->
            (* return JSON data *)
            let json = Printf.sprintf "{\"id\":%d}" new_id in
            (* return also the location of this new item *)             
            let h = rd.Wm.Rd.resp_headers in
            let h = Cohttp.Header.add_unless_exists h "location" 
                ("/items/" ^ (string_of_int new_id))
            in
            Lwt.return { rd with Wm.Rd.resp_body = `String json;
                                 resp_headers = h }
        )
    )

  (* Modify an item *)
  method private modify_item rd = 
    Log.method_called "modify_item" ;
    (* get the request body (should contains JSON) *)
    Cohttp_lwt_body.to_string rd.Wm.Rd.req_body >>= (
      fun v -> 
        Log.info (Printf.sprintf "Body content: %s" v) ;
        (* get the item id from the url *)
        let id = self#id rd in
        Log.info (Printf.sprintf "Item id: %d" id) ;
        (* modify the item *)
        Db.put self#get_db id v >>= (
          fun id ->
            (* return JSON data *)
            let json = 
              if id = 0 then
                "{\"status\":\"not found\"}"
              else
                "{\"status\":\"ok\"}"
            in
            Lwt.return { rd with Wm.Rd.resp_body = `String json }
        )
    )

  (* Delete an item *)
  method private delete_item rd = 
    Log.method_called "delete_item" ;
    (* get the item id from the url *)
    let id = self#id rd in
    Log.info (Printf.sprintf "Item id: %d" id) ;
    (* delete the item *)
    Db.delete self#get_db id >>= (
      fun id ->
        (* return JSON data *)
        let json = 
          if id = 0 then
            "{\"status\":\"not found\"}"
          else
            "{\"status\":\"ok\"}"
        in
        Lwt.return { rd with Wm.Rd.resp_body = `String json }
    )

  (* Get item(s) in JSON *)
  method private retrieve_item rd =
    Log.method_called "retrieve_item" ;
    (* if there is no id, get all the items *)
    let id = self#id rd in
    let items = 
      if (id = 0) then
        Db.get_all self#get_db
      else
        Db.get self#get_db id
    in
    let build_list acc (_, e) = 
      e :: acc
    in
    items >>= (fun items ->
        let json = List.rev(List.fold_left build_list [] items) in
        let json = String.concat ", " json in
        let json = Printf.sprintf "[%s]" json in
        Wm.continue (`String json) rd
      )    

end

let main () =
  let open Lwt.Infix in
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
  let callback (_, _) request body =
    try
      (* perform route dispatch *)
      Log.info "\nNew request" ;
      Wm.dispatch' routes ~body ~request
      >|= begin function
        | None        -> (
            `Not_found, Cohttp.Header.init (), `String "Not found", []
          )
        | Some result -> result
      end
      >>= fun (status, headers, body, _) ->
      (* send the response to the client *)
      Server.respond ~headers ~body ~status ()
    with
    | e -> Log.error (Printexc.to_string e); raise e
  in
  (* create the server and handle requests with the function defined above *)
  let config =
    Server.make ~callback ()
  in
  (* listen only on 127.0.0.1 ip address *)
  Conduit_lwt_unix.init ~src:"127.0.0.1" ()
  >>= fun ctx ->
  let ctx = Cohttp_lwt_unix_net.init ~ctx () in
  Server.create ~ctx ~mode:(`TCP(`Port port)) config
  >>= (fun () -> Lwt.return_unit)

let () =  Lwt_main.run (main ())
