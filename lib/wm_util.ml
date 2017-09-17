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
    | _, (_, x)::xs ->
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

  let type_of_string str =
    match Re_str.(split (regexp "/") str) with
      | [x; y] -> x, y
      | _      -> assert false
          (* FIXME: raise an exception to respond Invalid request to client ? *)

  let type_in_range (type_,subtype) range =
    let open Accept in
    match range with
    | AnyMedia                    -> true
    | AnyMediaSubtype type_'       -> type_' = type_
    | MediaType (type_', subtype') -> type_' = type_ && subtype' = subtype

  let media_match range (type_, _) =
    let type_ = type_of_string type_ in
    type_in_range type_ range

  let match_accept_header provided header =
    (* sort in descending order of quality *)
    let ranges = List.sort (fun (q1, _) (q2, _) -> compare q2 q1)
      Accept.(media_ranges header)
    in
    let rec loop = function
      | [] -> None
      | (_,(range,_))::rs ->
        try Some(List.find (media_match range) provided)
        with Not_found -> loop rs
    in
    loop ranges

  let match_provided_header accepted header =
    let accepted_ranges =
      (* keep order with fold_right *)
      List.fold_right
        (fun (str,f) acc ->
           match Accept.media_ranges (Some str) with
             [] -> acc
           | (_,(range,_)) :: _ -> (range, f) :: acc)
           accepted []
    in
    (* content type provided by client is unique *)
    let header_type = type_of_string header in
    let predicate (range,_) = type_in_range header_type range in
    try Some (List.find predicate accepted_ranges)
    with Not_found -> None
end

module Date = struct
  open CalendarLib

  let parse_rfc1123_date_exn s =
    Printer.Time.from_fstring "%a, %d %b %Y %H:%M:%S GMT" s

  let parse_rfc1123_date s =
    try (Some (parse_rfc1123_date_exn s)) with
    | Invalid_argument _ -> None
end
