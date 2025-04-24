open OUnit2

let test_get_bits_basic _ =
  for i = 0 to 4 do
	let res = Lzw.get_bits (Bytes.of_string "hello") (i * 8) 8 in
	assert_equal res (int_of_char (String.get "hello" i))
  done

let test_all_bit_width_1 _ =
  for v = 0 to 255 do
	for i = 0 to 8 do
	  let res = Lzw.get_bits (Bytes.init 1 (fun _ -> char_of_int v)) 0 i in
	  assert_equal res (v land (0xFF lsr (8 - i)))
	done
  done

let test_all_bit_width_2 _ =
  for v = 0 to 255 do
	for i = 0 to 7 do
	  let res =
		Lzw.get_bits (Bytes.init 1 (fun _ -> char_of_int v)) (7 - i) i
	  in
	  assert_equal res ((v lsr (7 - i)) land (0xFF lsr (8 - i)))
	done
  done

let test_get_larger_bits _ =
  let src = Lzw.flatten_codes 8 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8) ] in
  for v = 0 to 16 do
	let expected = 0xCDAB land ((1 lsl v) - 1) in
	let actual = Lzw.get_bits src 0 v in
	assert_equal expected actual
  done

let test_get_longer_slices _ =
  let src = Lzw.flatten_codes 8 [ (Z.of_int 0xAB, 8); (Z.of_int 0xCD, 8) ] in
  for v = 0 to 16 do
	let expected = (0xCDAB lsr (16 - v)) land ((1 lsl v) - 1) in
	let actual = Lzw.get_bits src (16 - v) v in
	assert_equal expected actual
  done


let suite =
"LZW"
>::: [
	   "Get bits of string" >:: test_get_bits_basic;
	   "Get all bits of char" >:: test_all_bit_width_1;
	   "Get all bits of char" >:: test_all_bit_width_2;
	   "Get bits bigger than byte" >:: test_get_larger_bits;
	   "Get bits bigger than byte with offset" >:: test_get_longer_slices;
	  ]

let () =
	run_test_tt_main suite
