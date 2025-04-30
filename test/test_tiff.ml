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
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 128) res

let test_load_data_as_wrong_type_fails fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument "datatype not correct for plane") (fun _ ->
      Tiff.data ~window tiff ro Tiff.Data.Float32)

let test_load_simple_int8_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_int8_lzw.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 8
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 10
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 8; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Int8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  (* uses alternating +ve and -ve values, so sum should be zero *)
  assert_equal ~printer:Int.to_string ~msg:"Value sum" 0 res

let test_load_simple_uint8_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_uint8_lzw.tiff"))
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
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  (* uses alternating +ve and -ve values, so sum should be zero *)
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 234) res

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
  assert_equal ~msg:"sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Int16 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 1234) res

let test_load_simple_uint16_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_uint16_lzw.tiff"))
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
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint16 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Value sum" (10 * 10 * 61234) res

let test_load_simple_int32_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_int32_lzw.tiff"))
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
  assert_equal ~msg:"sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Int32 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  (* sum of data is 0 as it is an equal mix of +ve and -ve values *)
  assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res

let test_load_simple_uint32_tiff fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_uint32_lzw.tiff"))
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
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  assert_raises ~msg:"Can't load uint32"
    (Invalid_argument "datatype not correct for plane") (fun _ ->
      Tiff.data ~window tiff ro Tiff.Data.Int32)

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
  assert_equal ~msg:"sample format" Tiff.Ifd.IEEEFloatingPoint
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Float32 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  (* We need to force OCaml to get the 32 bit float representation of the number *)
  let value = Int32.float_of_bits (Int32.bits_of_float 1.2345) in
  let expected = ref 0. in
  for _ = 0 to 99 do
    expected := !expected +. value
  done;
  assert_bool "Value sum" (cmp_float !expected res)

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
  assert_equal ~msg:"sample format" Tiff.Ifd.IEEEFloatingPoint
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Chunky (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Float64 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  let expected = ref 0. in
  for _ = 0 to 99 do
    expected := !expected +. 1.2345
  done;
  assert_bool "Value sum" (cmp_float !expected res)

let uniform_rgb_uint8_lzw fs _ =
  Eio.Path.(with_open_in (fs / "../testdata/uniform_rgb_uint8_lzw.tiff"))
  @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 10
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 10
    (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 3
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ; 8 ; 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~printer:Int.to_string ~msg:"Rows per strip" 10
    (Tiff.Ifd.rows_per_strip header);
  assert_equal ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c)) ~msg:"Planar configuration" Tiff.Ifd.Planar (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  assert_equal ~printer:Int.to_string ~msg:"Check channel 1" (1 * 10 * 10) res

let suite fs =
  "Basic tests"
  >::: [
         "Test load simple uniform uncompressed tiff"
         >:: test_load_uniform_tiff fs;
         "Test load data as wrong type"
         >:: test_load_data_as_wrong_type_fails fs;
         "Test load simple int8 tiff" >:: test_load_simple_int8_tiff fs;
         "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff fs;
         "Test load simple int16 tiff" >:: test_load_simple_int16_tiff fs;
         "Test load simple uint16 tiff" >:: test_load_simple_uint16_tiff fs;
         "Test load simple int32 tiff" >:: test_load_simple_int32_tiff fs;
         "Test load simple uint32 tiff" >:: test_load_simple_uint32_tiff fs;
         "Test load simple float32 tiff" >:: test_load_simple_float32_tiff fs;
         "Test load simple float64 tiff" >:: test_load_simple_float64_tiff fs;
         "Test load three channel tiff" >:: uniform_rgb_uint8_lzw fs;
       ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
