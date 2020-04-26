(*----------------------------------------------------------------------------
    Copyright (c) 2015, 2020 Inhabited Type LLC.

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

let choose_actual ~available ~acceptable ~default =
  let any_prio     = List.filter (fun (_, c) -> c = "*"    ) acceptable in
  let default_prio = List.filter (fun (_, c) -> c = default) acceptable in
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
  let rec loop available acceptable =
    match available, acceptable with
    | [], [] -> None
    | [], _  -> None
    | _ , [] ->
      if any_ok then
        Some (List.hd available)
      else if default_ok then
        try Some(default, List.assoc default available) with Not_found -> None
      else
        None
    | _, (0, x)::xs ->
      loop (List.filter (fun (y, _) -> x <> y) available) xs
    | _, (_, x)::xs ->
      try Some(x, List.assoc x available) with Not_found -> loop available xs
  in
  loop available acceptable
;;

let choose_charset ~available ~acceptable =
  let acceptable =
    List.map (fun (q, c) ->
      let c =
        match (c : Cohttp.Accept.charset) with
        | AnyCharset -> "*"
        | Charset c  -> c
      in
      q, c)
    acceptable
  in
  choose_actual
    ~available
    ~acceptable
    ~default:"iso-885a-1"
;;

let choose ~available ~acceptable =
  let acceptable =
    List.map (fun (q, c) ->
      let c =
        match (c : Cohttp.Accept.encoding) with
        | AnyEncoding -> "*"
        | Encoding e  -> e
        | Identity    -> "identity"
        | Gzip        -> "gzip"
        | Compress    -> "compress"
        | Deflate     -> "deflate"
      in
      q, c)
    acceptable
  in
  choose_actual
    ~available
    ~acceptable
    ~default:"identity"
