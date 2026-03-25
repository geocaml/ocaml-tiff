open Bechamel

let pad width v =
  let padding = String.make (width - String.length v) ' ' in
  padding ^ v

let pp_row ~width f data =
  data
  |> List.iteri (fun i v ->
      if i > 0 then Fmt.comma f ();
      Fmt.pf f "%s" (pad width.(i) v))

let pp f results =
  let results = Hashtbl.to_seq results |> Array.of_seq in
  Array.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) results;
  (* Sort columns *)
  let rows = snd results.(0) |> Hashtbl.to_seq |> Seq.map fst |> Array.of_seq in
  let width = Array.make (Array.length results + 1) 0 in
  width.(0) <- String.length "name, ";
  results
  |> Array.iteri (fun i (name, _) -> width.(i + 1) <- String.length name + 2);
  Array.sort String.compare rows;
  let rows =
    rows
    |> Array.map (fun name ->
        width.(0) <- max width.(0) (String.length name + 2);
        let values =
          results
          |> Array.mapi (fun i (_, col) ->
              let v =
                match Hashtbl.find col name |> Analyze.OLS.estimates with
                | Some [ v ] -> Printf.sprintf "%f" v
                | _ -> assert false
              in
              width.(i + 1) <- max width.(i + 1) (String.length v + 2);
              v)
        in
        (name, values))
  in
  let metrics = Array.to_list results |> List.map fst in
  let headings = List.mapi (fun i v -> pad width.(i) v) ("name" :: metrics) in
  Fmt.pf f "@[<v>@[<h>%a@]" Fmt.(list ~sep:comma string) headings;
  rows
  |> Array.iter (fun (name, data) ->
      Fmt.pf f "@,@[<h>%a@]" (pp_row ~width) (name :: Array.to_list data));
  Fmt.pf f "@]"

(* Copyright (C) 2020-2021 Anil Madhavapeddy
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
