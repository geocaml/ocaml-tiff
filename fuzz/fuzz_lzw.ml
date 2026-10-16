(* Fuzz the LZW decoder: arbitrary input must fail cleanly, never crash *)

open Alcobar

(* The output blit is unchecked, so the buffer must fit the worst case: each
   pass reads >= 9 bits and writes one < 4096 byte entry, i.e. n * 4096 *)
let max_input = 4096 (* cap input to bound the buffer *)

let decode_never_crashes s =
  let s = if String.length s > max_input then String.sub s 0 max_input else s in
  let input = Cstruct.of_string s in
  let output = Cstruct.create (String.length s * 4096) in
  try Tiff.Private.Lzw.decode input output
  with Invalid_argument _ | End_of_file | Failure _ -> ()

let suite =
  ("lzw", [ test_case "decode never crashes" [ bytes ] decode_never_crashes ])
