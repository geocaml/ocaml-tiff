(* DEFLATE decompression for TIFF files using the decompress library *)

module Unsafe = struct
  external unsafe_blit_string_to_bigstring :
    string -> int -> Cstruct.buffer -> int -> int -> unit
    = "caml_blit_string_to_bigstring"
  [@@noalloc]

  let blit_string src srcoff (dst : Cstruct.t) dstoff len =
    unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off + dstoff) len
end

let decode input output_buffer =
  let input_len = Cstruct.length input in
  let output_len = Cstruct.length output_buffer in

  (* Convert Cstruct to string for decompress library *)
  let input_string = Cstruct.to_string input in

  (* Use the Higher interface for DEFLATE decompression *)
  let input_pos = ref 0 in
  let output_pos = ref 0 in

  let refill buf =
    let remaining = input_len - !input_pos in
    if remaining > 0 then (
      let to_read = min remaining (Bigstringaf.length buf) in
      let input_substring = String.sub input_string !input_pos to_read in
      Bigstringaf.blit_from_string ~src_off:0 ~dst_off:0 ~len:to_read
        input_substring buf;
      input_pos := !input_pos + to_read;
      to_read)
    else 0
  in

  let flush buf len =
    let to_write = min len (output_len - !output_pos) in
    if to_write > 0 then (
      let output_string = Bigstringaf.substring buf ~off:0 ~len:to_write in
      Unsafe.blit_string output_string 0 output_buffer !output_pos to_write;
      output_pos := !output_pos + to_write)
  in

  (* Create a window for decompression *)
  let window = De.make_window ~bits:15 in

  (* Create input and output bigstrings *)
  let input_bigstring : De.bigstring =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout input_len
  in
  let output_bigstring : De.bigstring =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout output_len
  in

  (* Copy input string to bigstring *)
  for i = 0 to input_len - 1 do
    Bigarray.Array1.set input_bigstring i input_string.[i]
  done;

  (* Use the Higher interface *)
  match
    De.Higher.uncompress ~refill ~flush ~w:window input_bigstring
      output_bigstring
  with
  | Ok () -> ()
  | Error (`Msg err) -> failwith ("DEFLATE decompression error: " ^ err)
