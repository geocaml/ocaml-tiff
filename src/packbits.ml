(* Implements the PackBits decompression algorithm. *)

let decode (input : Cstruct.t) (output_buffer : Cstruct.t) =
  let in_len = Cstruct.length input in
  let out_len = Cstruct.length output_buffer in

  let read_byte pos =
    if pos >= in_len then
      Error.raise_error Error.Malformed_file "PackBits: unexpected end of input";
    Cstruct.get_uint8 input pos
  in

  let write_byte pos b =
    if pos >= out_len then
      Error.raise_error Error.Malformed_file "PackBits: output buffer too small";
    Cstruct.set_uint8 output_buffer pos b
  in

  let rec loop in_pos out_pos =
    if out_pos = out_len then
      begin if in_pos <> in_len then
        Error.raise_error Error.Malformed_file
          "PackBits: trailing data after output buffer is full"
      end
    else begin
      let header = read_byte in_pos in
      let next_in = in_pos + 1 in

      if header <= 127 then begin
        let count = header + 1 in
        let rec copy i in_p out_p =
          if i = count then loop in_p out_p
          else begin
            if out_p >= out_len then
              Error.raise_error Error.Malformed_file
                "PackBits: literal run exceeds output buffer";
            let byte = read_byte in_p in
            write_byte out_p byte;
            copy (i + 1) (in_p + 1) (out_p + 1)
          end
        in
        copy 0 next_in out_pos
      end
      else if header >= 129 then begin
        let count = 257 - header in
        let byte = read_byte next_in in
        let rec repeat i out_p =
          if i = count then loop (next_in + 1) out_p
          else begin
            if out_p >= out_len then
              Error.raise_error Error.Malformed_file
                "PackBits: replicate run exceeds output buffer";
            write_byte out_p byte;
            repeat (i + 1) (out_p + 1)
          end
        in
        repeat 0 out_pos
      end
      else loop next_in out_pos
    end
  in

  loop 0 0
