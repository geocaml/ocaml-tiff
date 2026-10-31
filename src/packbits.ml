(* Implements the PackBits decompression algorithm.
   
   Note: The TIFF spec recommends compressing rows independently.
   Decoding the whole strip as one stream gives identical output for
   spec-compliant files and tolerates encoders that let runs cross
   row boundaries. The tradeoff is that a corrupt row boundary in a
   non-compliant file won't be detected. *)

let decode (input : Cstruct.t) (output_buffer : Cstruct.t) =
  let in_len = Cstruct.length input in
  let out_len = Cstruct.length output_buffer in

  if out_len = 0 && in_len > 0 then
    Error.raise_error Error.Malformed_file
      "PackBits: zero-size output buffer with non-empty input";

  let read_byte pos =
    if pos >= in_len then
      Error.raise_error Error.Malformed_file "PackBits: unexpected end of input";
    Cstruct.get_uint8 input pos
  in

  (* Defensive check: should never trigger, but guards against logic errors *)
  let write_byte pos b =
    if pos >= out_len then
      Error.raise_error Error.Malformed_file "PackBits: output buffer too small";
    Cstruct.set_uint8 output_buffer pos b
  in

  (* Loop until the output buffer is full. Any leftover input is ignored,
     which tolerates files with trailing padding. *)
  let rec loop in_pos out_pos =
    if out_pos = out_len then ()
    else
      let header = read_byte in_pos in
      let next_in = in_pos + 1 in

      if header <= 127 then begin
        (* Literal run: copy the next (header + 1) bytes *)
        let count = header + 1 in
        let rec copy i in_p out_p =
          (* If we hit the end of the output buffer mid-run, we stop.
             This truncates the last run, which tolerates encoders that
             overshoot the expected size. *)
          if i = count || out_p = out_len then loop in_p out_p
          else
            let byte = read_byte in_p in
            write_byte out_p byte;
            copy (i + 1) (in_p + 1) (out_p + 1)
        in
        copy 0 next_in out_pos
      end
      else if header >= 129 then begin
        (* Replicate run: repeat the next byte (257 - header) times *)
        let count = 257 - header in
        let byte = read_byte next_in in
        let rec repeat i out_p =
          if i = count || out_p = out_len then loop (next_in + 1) out_p
          else (
            write_byte out_p byte;
            repeat (i + 1) (out_p + 1))
        in
        repeat 0 out_pos
      end
      else (* header = 128, a no-op *)
        loop next_in out_pos
  in

  loop 0 0
