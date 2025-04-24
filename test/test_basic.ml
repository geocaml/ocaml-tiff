open OUnit2

let test_load_uniform_tiff _ =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
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

let test_load_uniform_lzw_tiff _ =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
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

let test_load_simple_uint8_tiff _ =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
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

let test_load_simple_float32_geotiff _ =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
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

let test_load_simple_float32_tiff _ =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
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
  assert_equal ~msg:"Value sum" 50. res

let suite =
  "Basic tests"
  >::: [
         "Test load simple uniform tiff" >:: test_load_uniform_tiff;
         "Test load simple uniform LZW tiff" >:: test_load_uniform_lzw_tiff;
         "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff;
         "Test load simple float32 tiff" >:: test_load_simple_float32_tiff;
         "Test load simple float32 geotiff" >:: test_load_simple_float32_geotiff;
       ]

let () = run_test_tt_main suite
