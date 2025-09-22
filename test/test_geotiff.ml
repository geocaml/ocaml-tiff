open OUnit2

let cmp_float_list a b =
  List.fold_left2 (fun acc a b -> acc && cmp_float a b) true a b

let test_load_simple_uint8_tiff fs _ =
  Eio.Path.(with_open_in (fs / "./data/cea.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file Tiff.Uint8 ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 514
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 515
    (Tiff.Ifd.height header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  let pixel_scale = Array.to_list (Tiff.Ifd.pixel_scale header) in
  let expected = [ 60.022136983193739; 60.022136983193739; 0.0 ] in
  assert_bool "Pixel width" (cmp_float_list expected pixel_scale);

  let tie_point = Array.to_list (Tiff.Ifd.tiepoint header) in
  let expected =
    [ 0.; 0.; 0.; -28493.166784412522247; 4255884.543802191503346; 0. ]
  in
  assert_bool "Pixel width" (cmp_float_list expected tie_point);

  assert_equal ~msg:"ascii params" [ "unnamed"; "NAD27" ]
    (Tiff.Ifd.geo_ascii_params header)

let test_load_simple_float32_geotiff fs _ =
  Eio.Path.(with_open_in (fs / "./data/aoh.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file Tiff.Float32 ro in
  let header = Tiff.ifd tiff in
  assert_equal ~printer:Int.to_string ~msg:"Image width" 7
    (Tiff.Ifd.width header);
  assert_equal ~printer:Int.to_string ~msg:"Image height" 3
    (Tiff.Ifd.height header);
  assert_equal ~printer:Int.to_string ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 32 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.IEEEFloatingPoint
    (Tiff.Ifd.sample_format header);

  let pixel_scale = Array.to_list (Tiff.Ifd.pixel_scale header) in
  let expected = [ 0.016666666666667; 0.016666666666667; 0.0 ] in
  assert_bool "Pixel width" (cmp_float_list expected pixel_scale);

  let tie_point = Array.to_list (Tiff.Ifd.tiepoint header) in
  let expected = [ 0.; 0.; 0.; -73.566666666664560; 22.637233333845138; 0. ] in
  assert_bool "Pixel width" (cmp_float_list expected tie_point);

  assert_equal ~msg:"ascii params" [ "WGS 84" ]
    (Tiff.Ifd.geo_ascii_params header)

let suite fs =
  "Basic tests"
  >::: [
         "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff fs;
         "Test load simple float32 geotiff"
         >:: test_load_simple_float32_geotiff fs;
       ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
