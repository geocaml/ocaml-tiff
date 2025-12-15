(* Implements the LZW decompression algorithm *)

(* In TIFF and GIF LZW chars are at most 12 bits *)
type lzwstring = string

module Unsafe = struct
  external unsafe_blit_string_to_bigstring :
    string -> int -> Cstruct.buffer -> int -> int -> unit
    = "caml_blit_string_to_bigstring"
  [@@noalloc]

  let blit_string src srcoff (dst : Cstruct.t) dstoff len =
    unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off + dstoff) len
end

let rec get_bits bytedata offset count =
  match count with
  | 0 -> 0
  | n ->
      let byte = int_of_char (Cstruct.get_char bytedata (offset / 8)) in
      let bitoffset = offset mod 8 in
      let bit = (byte lsr (7 - bitoffset)) land 1 in
      (bit lsl (n - 1)) + get_bits bytedata (offset + 1) (n - 1)

let build_table_entry ~previous next =
  if String.(equal next empty) then previous
  else previous ^ String.make 1 next.[0]

let update_buffer buffer offset codestr =
  let len = String.length codestr in
  Unsafe.blit_string codestr 0 buffer offset len;
  offset + len

let max_dict_size = 4096

let init_table () =
  Array.init max_dict_size (function
    | i when i < 256 -> String.make 1 (Char.chr i)
    | _ -> String.empty)

let reset_table dict =
  Array.iteri
    (fun i _ ->
      (* Only non-ASCII characters need resetting *)
      if i >= 256 then Array.set dict i String.empty)
    dict

let decode input output_buffer =
  let dict : lzwstring array = init_table () in

  let rec inner in_offset out_offset code_size next_code_index
      (prev_code : lzwstring) =
    let code = get_bits input in_offset code_size in
    match code with
    | 257 -> () (* end code *)
    | 256 -> (
        (* clear code *)
        reset_table dict;
        let next_code = get_bits input (in_offset + code_size) 9 in
        match next_code with
        | 257 -> ()
        | _ ->
            let next = String.make 1 (Char.chr (next_code land 0xFF)) in
            let out_offset = update_buffer output_buffer out_offset next in
            inner (in_offset + code_size + 9) out_offset 9 258 next)
    | _ ->
        let entry =
          if code < next_code_index then (
            let entry = dict.(code) in
            if next_code_index < max_dict_size then
              dict.(next_code_index) <-
                build_table_entry ~previous:prev_code entry;
            entry)
          else
            let e = build_table_entry ~previous:prev_code prev_code in
            if next_code_index < max_dict_size then dict.(next_code_index) <- e;
            e
        in

        let prev_entry, new_code_size, new_code_index =
          match next_code_index with
          | 4095 -> (prev_code, code_size, next_code_index)
          | _ ->
              let new_code_size =
                (* We increase the code_size after inserting the 2^code_size - 1 element
                   into the dictionary *)
                match next_code_index with
                | 510 | 1022 | 2046 -> code_size + 1
                | _ -> code_size
              in
              (entry, new_code_size, next_code_index + 1)
        in

        let out_offset = update_buffer output_buffer out_offset entry in
        inner (in_offset + code_size) out_offset new_code_size new_code_index
          prev_entry
  in
  inner 0 0 9 258 String.empty
