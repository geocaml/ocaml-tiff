open OUnit2

let test_load_uniform_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 256
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 256
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.No_compression
    (Tiff.Ifd.compression header);
  assert_raises ~msg:"Samples per pixel" Not_found (fun () ->
      Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_raises ~msg:"Predictor" Not_found (fun () -> Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 128) res

let test_load_uniform_lzw_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_lzw.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 10
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 10
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_raises ~msg:"Predictor" Not_found (fun () -> Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 128) res

let test_load_simple_uint8_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/cea.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 514
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 515
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.No_compression
    (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_raises ~msg:"Predictor" Not_found (fun () -> Tiff.Ifd.predictor header);
  assert_equal ~msg:"Pixel width"
    [| 60.022136983193739; 60.022136983193739; 0.0 |]
    (Tiff.Ifd.pixel_scale header)

let test_load_simple_float32_geotiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/aoh.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 7
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 3
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 32 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 3
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_equal ~msg:"Pixel width"
    [| 0.016666666666667; 0.016666666666667; 0.0 |]
    (Tiff.Ifd.pixel_scale header)

let test_load_simple_float32_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_float32_lzw.tiff"))
  @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 10
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 10
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 32 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Float32 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  (* We need to force OCaml to get the 32 bit float representation of the number *)
  let value = Int32.float_of_bits (Int32.bits_of_float 1.2345) in
  let expected = ref 0. in
  for _ = 0 to 99 do
    expected := !expected +. value
  done;
  assert_equal ~printer:Float.to_string ~msg:"Value sum" !expected res

let test_load_simple_float64_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_float64_lzw.tiff"))
  @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 10
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 10
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 64 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Float64 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  let expected = ref 0. in
  for _ = 0 to 99 do
    expected := !expected +. 1.2345
  done;
  assert_equal ~printer:(fun f -> Printf.sprintf "%H" f) ~msg:"Value sum" !expected res

let test_load_simple_int16_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_int16_lzw.tiff"))
  @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 10
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 10
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 16 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Int16 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 1234) res

let suite fs =
  "Basic tests"
  >::: [
         "Test load simple uniform tiff" >:: test_load_uniform_tiff fs;
         "Test load simple uniform LZW tiff" >:: test_load_uniform_lzw_tiff fs;
         "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff fs;
         "Test load simple float32 tiff" >:: test_load_simple_float32_tiff fs;
         "Test load simple float64 tiff" >:: test_load_simple_float64_tiff fs;
         "Test load simple float32 geotiff"
         >:: test_load_simple_float32_geotiff fs;
         "Test load simple int16 tiff" >:: test_load_simple_int16_tiff fs;
       ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
