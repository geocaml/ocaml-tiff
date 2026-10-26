(* Tests the correctness of the LZW decompression algorithm with pre-compressed test data *)

open OUnit2

let test_get_bits_basic _ =
  for i = 0 to 4 do
    let res = Tiff.Private.Lzw.get_bits (Cstruct.of_string "hello") (i * 8) 8 in
    assert_equal res (int_of_char (String.get "hello" i))
  done

let test_all_bit_width _ =
  let buffer = Cstruct.create 1 in
  for v = 0 to 255 do
    Cstruct.set_char buffer 0 (char_of_int v);
    for i = 0 to 8 do
      let res = Tiff.Private.Lzw.get_bits buffer 0 i in
      let expected = v lsr (8 - i) in
      assert_equal ~printer:Int.to_string expected res
    done
  done

let test_decode_output_too_small _ =
  (* 9-bit codes Clear, 'A', 'B', End: decodes to "AB" *)
  let input = Cstruct.of_string "\x80\x10\x48\x50\x10" in
  let out = Cstruct.create 2 in
  Tiff.Private.Lzw.decode input out;
  assert_equal ~printer:Fun.id "AB" (Cstruct.to_string out);
  match Tiff.Private.Lzw.decode input (Cstruct.sub out 0 1) with
  | () -> assert_failure "decode wrote past the end of its output buffer"
  | exception Invalid_argument _ -> ()

let suite =
  "LZW"
  >::: [
         "Get bits of string" >:: test_get_bits_basic;
         "Get all bits of char" >:: test_all_bit_width;
         "Decode into too small a buffer" >:: test_decode_output_too_small;
       ]

let () = run_test_tt_main suite
