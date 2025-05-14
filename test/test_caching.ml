open OUnit2

let test_caching_none _ =
  let path = "../testdata/uniform_uint8_lzw.tiff" in
  Tiff_unix.with_open_in path @@ fun ro ->
  let tiff = Tiff.from_file ro in

  let read_count = ref 0 in
  let counting_ro ~file_offset blocks =
    read_count := !read_count + 1;
    ro ~file_offset blocks
  in

  let data = Tiff.data tiff counting_ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 234) res;
  assert_equal ~printer:Int.to_string ~msg:"Read count" 1 !read_count;

  read_count := 0;
  let data = Tiff.data tiff counting_ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 234) res;
  assert_equal ~printer:Int.to_string ~msg:"Read count" 1 !read_count

let test_caching_all _ =
  let path = "../testdata/uniform_uint8_lzw.tiff" in
  Tiff_unix.with_open_in path @@ fun ro ->
  let tiff = Tiff.from_file ~caching_policy:Tiff.CacheAll ro in

  let read_count = ref 0 in
  let counting_ro ~file_offset blocks =
    read_count := !read_count + 1;
    ro ~file_offset blocks
  in

  let data = Tiff.data tiff counting_ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 234) res;
  assert_equal ~printer:Int.to_string ~msg:"Read count" 1 !read_count;

  read_count := 0;
  let data = Tiff.data tiff counting_ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 234) res;
  assert_equal ~printer:Int.to_string ~msg:"Read count" 0 !read_count

let suite =
  "Caching"
  >::: [
         "Test load without caching strips" >:: test_caching_none;
         "Test load caching all strips" >:: test_caching_all;
       ]

let () = run_test_tt_main suite
