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
  let any_prio     = List.filter (fun (_, c) -> c = "*"    ) accepted in
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
    | [] | [0, _] -> false
    | _           -> true
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

  let media_match (_, (range, _)) (type_, _) =
    let type_, subtype =
      match Re.Str.(split (regexp "/") type_) with
      | [x; y] -> x, y
      | _      -> assert false
    in
    let open Accept in
    match range with
    | AnyMedia                     -> true
    | AnyMediaSubtype type_'       -> type_' = type_
    | MediaType (type_', subtype') -> type_' = type_ && subtype' = subtype

  let match_header provided header =
    let ranges = try Accept.(media_ranges header |> qsort) with Parsing.Parse_error -> [] in
    let rec loop = function
      | [] -> None
      | r::rs -> try Some(List.find (media_match r) provided) with Not_found -> loop rs
    in
    loop ranges
end

module Date = struct
  let parse_rfc1123_date_exn s =
    try Scanf.sscanf s "%3s, %d %s %4d %d:%d:%d %s" (
      fun _wday mday mon year hour min sec tz ->
        let months = [
          "Jan", 1; "Feb", 2; "Mar", 3; "Apr", 4; "May", 5; "Jun", 6;
          "Jul", 7; "Aug", 8; "Sep", 9; "Oct", 10; "Nov", 11; "Dec", 12
        ] in
        let parse_tz = function
          | "" | "Z" | "GMT" | "UTC" | "UT" -> 0
          | "PST" -> -480
          | "MST" | "PDT" -> -420
          | "CST" | "MDT" -> -360
          | "EST" | "CDT" -> -300
          | "EDT" -> -240
          | s -> Scanf.sscanf s "%c%02d%_[:]%02d" (fun sign hour min ->
              min + hour * (if sign = '-' then -60 else 60))
        in
        let mon = List.assoc mon months in
        let year =
          if year < 50 then year + 2000
          else if year < 1000 then year + 1900
          else year
        in
        let date = (year, mon, mday) in
        let time = ((hour, min, sec), (parse_tz tz)*60) in
        let ptime = Ptime.of_date_time (date, time) in
        match ptime with
          | None -> raise @@ Invalid_argument "Invalid date string"
          | Some date ->
        match Ptime.(Span.to_int_s (to_span date)) with
          | None -> raise @@ Invalid_argument "Invalid date string"
          | Some t -> t
    )
    with
      | Scanf.Scan_failure e -> raise @@ Invalid_argument e
      | Not_found -> raise @@ Invalid_argument "Invalid date string"

  let parse_rfc1123_date s =
    try (Some (parse_rfc1123_date_exn s)) with
    | Invalid_argument _ -> None
end
