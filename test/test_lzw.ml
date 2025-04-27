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

let suite =
  "LZW"
  >::: [
         "Get bits of string" >:: test_get_bits_basic;
         "Get all bits of char" >:: test_all_bit_width;
       ]

let () = run_test_tt_main suite
