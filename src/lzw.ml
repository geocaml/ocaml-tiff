(* In TIFF and GIF LZW chars are at most 12 bits *)
type lzwchar = char
type lzwstring = lzwchar list

let rec get_bits bytedata offset count =
  match count with
  | 0 -> 0
  | n ->
      let byte = int_of_char (Cstruct.get_char bytedata (offset / 8)) in
      let bitoffset = offset mod 8 in
      let bit = (byte lsr (7 - bitoffset)) land 1 in
      (bit lsl (n - 1)) + get_bits bytedata (offset + 1) (n - 1)

let build_table_entry a b =
  match b with [] -> a | x :: _ -> List.rev (x :: List.rev a)

let update_buffer buffer offset codestr =
  List.fold_left
    (fun acc c ->
      Cstruct.set_char buffer acc c;
      acc + 1)
    offset codestr

let decode input output_buffer =
  let dict : lzwstring array = Array.make 4096 [] in

  let rec inner in_offset out_offset code_size next_code_index prev_code =
    let code = get_bits input in_offset code_size in
    match code with
    | 257 -> () (* end code *)
    | 256 -> (
        (* clear code *)
        let next_code = get_bits input (in_offset + code_size) 9 in
        match next_code with
        | 257 -> ()
        | _ ->
            let out_offset =
              update_buffer output_buffer out_offset
                [ Char.chr (next_code land 0xFF) ]
            in
            inner
              (in_offset + code_size + 9)
              out_offset 9 258
              [ Char.chr (next_code land 0xFF) ])
    | _ ->
        let entry =
          if code < 256 then [ Char.chr (code land 0xFF) ]
          else if code < next_code_index then dict.(code)
          else build_table_entry prev_code prev_code
        in

        if next_code_index < 4096 then
          dict.(next_code_index) <- build_table_entry prev_code entry;
        let prev_entry, new_code_size, new_code_index =
          match next_code_index with
          | 4095 -> (prev_code, code_size, next_code_index)
          | _ ->
              let i = next_code_index + 1 in
              ( entry,
                (if i >= (1 lsl code_size) - 1 then code_size + 1 else code_size),
                i )
        in

        let out_offset = update_buffer output_buffer out_offset entry in
        inner (in_offset + code_size) out_offset new_code_size new_code_index
          prev_entry
  in
  inner 0 0 9 258 []
