(* DEFLATE/Zlib decompression for TIFF files using the decompress library *)

let decode input output_buffer =
  let input_len = Cstruct.length input in
  let output_len = Cstruct.length output_buffer in

  if input_len = 0 then failwith "Empty DEFLATE input"
  else if output_len = 0 then failwith "Empty DEFLATE output buffer";

  (* Access the underlying buffers directly - no string copies! *)
  let input_bigarray = Cstruct.to_bigarray input in
  let output_bigarray = Cstruct.to_bigarray output_buffer in

  (* Mutable position trackers *)
  let input_pos = ref 0 in
  let output_pos = ref 0 in

  (* Define refill and flush callbacks for Zl.Higher.uncompress *)
  (* Convert bigarrays to Bigstringaf.t for efficient blitting with offsets.
     This is safe because Cstruct.to_bigarray returns the same underlying type
     as Bigstringaf.t (both are Bigarray.Array1.t with char/int8_unsigned_elt). *)
  let input_bigstring = (Obj.magic input_bigarray : Bigstringaf.t) in
  let output_bigstring = (Obj.magic output_bigarray : Bigstringaf.t) in

  let refill buf =
    let remaining = input_len - !input_pos in
    if remaining = 0 then 0
    else
      let to_read = min remaining (Bigstringaf.length buf) in
      (* Blit directly from input_bigstring to buf using offsets - no sub needed *)
      Bigstringaf.blit input_bigstring ~src_off:!input_pos buf ~dst_off:0 ~len:to_read;
      input_pos := !input_pos + to_read;
      to_read
  in

  let flush buf len =
    let to_write = min len (output_len - !output_pos) in
    if to_write > 0 then (
      (* Blit directly from buf to output_bigstring using offsets - no sub needed *)
      Bigstringaf.blit buf ~src_off:0 output_bigstring ~dst_off:!output_pos ~len:to_write;
      output_pos := !output_pos + to_write)
  in

  let allocate bits = De.make_window ~bits in
  match
    Zl.Higher.uncompress ~allocate ~refill ~flush input_bigarray output_bigarray
  with
  | Ok () -> ()
  | Error (`Msg err) -> failwith ("DEFLATE decompression error: " ^ err)
