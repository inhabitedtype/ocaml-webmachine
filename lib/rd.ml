open Cohttp

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

let make
    ?(dispatch_path="")
    ?(path_info=[])
    ?(resp_headers=Header.init ())
    ?(resp_body=`Empty)
    ?(resp_redirect=false)
    ?(req_body=`Empty)
    ~request
    ()
  =
  { uri     = Uri.of_string request.Request.resource
  ; version = request.Request.version
  ; meth    = request.Request.meth
  ; req_headers = request.Request.headers
  ; resp_headers
  ; req_body
  ; resp_body
  ; resp_redirect
  ; dispatch_path
  ; path_info
  }

let with_req_headers f t =
  { t with req_headers = f t.req_headers }

let with_resp_headers f t =
  { t with resp_headers = f t.resp_headers }

let lookup_path_info_exn key t =
  List.assoc key t.path_info

let lookup_path_info key t =
  try Some(lookup_path_info_exn key t) with Not_found -> None

let redirect location t =
  with_resp_headers (fun header ->
    Header.add header "location" location)
  { t with resp_redirect = true }
