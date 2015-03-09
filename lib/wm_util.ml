let re_split_ws =
  let open Re in
  let space = greedy (rep space) in
  fun t -> split (compile (seq [space; t; space]))
;;

let choose (choices : (string * _) list) (accepted : (int * string) list) (default:string) =
  let any_prio = List.filter (fun (_, c) -> c = "*") accepted in
  let default_prio = List.filter (fun (_, c) -> c = default) accepted in
  let default_ok =
    match default_prio with
    | [] ->
      begin match any_prio with
      | [0, _] -> false
      | _   -> true
      end
    | [0, _] -> false
    | _ -> true
  in
  let any_ok =
    match any_prio with
    | []  -> false
    | [0, _] -> false
    | _   -> true
  in
  let rec loop choices accepted =
    match choices, accepted with
    | [], [] -> None
    | [], _  -> None
    | _ , [] ->
      if any_ok then
        Some (List.hd choices)
      else if default_ok then
        try Some(default, List.assoc default choices) with Not_found -> None
      else
        None
    | _, (0, x)::xs ->
      loop (List.filter (fun (y, _) -> x <> y) choices) xs
    | _, (p, x)::xs ->
      try Some(x, List.assoc x choices) with Not_found -> loop choices xs
  in
  loop choices accepted
;;

module ETag = struct
  let escape etag =
    Printf.sprintf "%s" etag

  let unescape s =
    Scanf.sscanf ("\"" ^ s ^ "\"") "%S" (fun u -> u)

  let from_header = function
    | None   -> []
    | Some s ->
      List.map unescape (re_split_ws (Re.char ',') s)
end

module MediaType = struct
  open Cohttp

  let compare_q (q1,_) (q2,_) =
    compare q1 q2

  let media_match (type_, _) =
    let type_, subtype = match Re_str.(split (regexp "/") type_) with
    | [type_; subtype] -> type_, subtype
    | _ -> assert false
    in
    let open Accept in
    function
    | _, (AnyMedia                    , _)  -> true
    | _, (AnyMediaSubtype type_'      , _) -> type_ = type_'
    | _, (MediaType (type_', subtype'), _) -> type_ = type_' && subtype = subtype'

  let match_header provided header =
    let ranges = List.sort compare_q Accept.(media_ranges header) in
    let pred m = List.exists (media_match m) ranges in
    try Some(List.find pred provided) with Not_found -> None
end
