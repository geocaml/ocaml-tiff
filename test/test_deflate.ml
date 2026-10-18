(* Round-trip a zlib payload through [Tiff.Private.Deflate.decode].

   The payload is larger than [De.io_buffer_size] once inflated, so the decoder
   has to flush its scratch buffer more than once. *)

open OUnit2

let inflated_len = (De.io_buffer_size * 2) + 100

let read_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let n = in_channel_length ic in
  really_input_string ic n

let payload () = Cstruct.of_string (read_file "data/deflate_multiflush.zlib")
let expected_byte i = Char.chr (i * 17 land 255)

let mismatches out =
  let bad = ref 0 in
  for i = 0 to Cstruct.length out - 1 do
    if Cstruct.get_char out i <> expected_byte i then incr bad
  done;
  !bad

let test_roundtrip_across_flushes _ =
  let out = Cstruct.create inflated_len in
  Tiff.Private.Deflate.decode (payload ()) out;
  assert_equal ~printer:string_of_int 0 (mismatches out)

let test_rejects_short_output _ =
  let out = Cstruct.create 100 in
  assert_raises (Failure "DEFLATE data exceeds strip size") (fun () ->
      Tiff.Private.Deflate.decode (payload ()) out)

let test_rejects_long_output _ =
  let out = Cstruct.create (inflated_len + 10) in
  assert_raises
    (Failure
       (Printf.sprintf "DEFLATE strip length %d, expected %d" inflated_len
          (inflated_len + 10)))
    (fun () -> Tiff.Private.Deflate.decode (payload ()) out)

let test_rejects_empty_input _ =
  assert_raises (Failure "Empty DEFLATE input") (fun () ->
      Tiff.Private.Deflate.decode Cstruct.empty (Cstruct.create 8))

let test_rejects_malformed_input _ =
  let bad = Cstruct.of_string "this is not a zlib stream" in
  try
    Tiff.Private.Deflate.decode bad (Cstruct.create 32);
    assert_failure "malformed input should fail"
  with Failure msg ->
    assert_bool msg
      (String.starts_with ~prefix:"DEFLATE decompression error" msg)

let suite =
  "DEFLATE"
  >::: [
         "Round-trip across several flushes" >:: test_roundtrip_across_flushes;
         "Reject output that is too small" >:: test_rejects_short_output;
         "Reject output that is too large" >:: test_rejects_long_output;
         "Reject empty input" >:: test_rejects_empty_input;
         "Reject malformed input" >:: test_rejects_malformed_input;
       ]

let () = run_test_tt_main suite
