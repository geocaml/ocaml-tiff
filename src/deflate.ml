(* Inflate one zlib (RFC 1950) TIFF strip.

   [Zl.Higher.uncompress] treats its two bigarrays as scratch space and writes
   the next chunk over the output scratch buffer after every flush. Those
   buffers therefore cannot be the caller's compressed strip or pixel buffer. *)

let decode input output_buffer =
  let input_len = Cstruct.length input in
  let output_len = Cstruct.length output_buffer in
  if input_len = 0 then failwith "Empty DEFLATE input"
  else if output_len = 0 then failwith "Empty DEFLATE output buffer";

  let input_ba = Cstruct.to_bigarray input in
  let output_ba = Cstruct.to_bigarray output_buffer in
  let scratch_in = De.bigstring_create De.io_buffer_size in
  let scratch_out = De.bigstring_create De.io_buffer_size in
  let input_pos = ref 0 in
  let output_pos = ref 0 in

  let refill buf =
    let remaining = input_len - !input_pos in
    if remaining = 0 then 0
    else
      let to_read = min remaining (Bstr.length buf) in
      Bstr.blit input_ba ~src_off:!input_pos buf ~dst_off:0 ~len:to_read;
      input_pos := !input_pos + to_read;
      to_read
  in

  let flush buf len =
    let next = !output_pos + len in
    if next > output_len then failwith "DEFLATE data exceeds strip size"
    else (
      Bstr.blit buf ~src_off:0 output_ba ~dst_off:!output_pos ~len;
      output_pos := next)
  in

  let allocate bits = De.make_window ~bits in
  match
    Zl.Higher.uncompress ~allocate ~refill ~flush scratch_in scratch_out
  with
  | Error (`Msg err) -> failwith ("DEFLATE decompression error: " ^ err)
  | Ok () ->
      if !output_pos <> output_len then
        failwith
          (Printf.sprintf "DEFLATE strip length %d, expected %d" !output_pos
             output_len)
