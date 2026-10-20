open OUnit2

let assert_equal_int = assert_equal ~printer:Int.to_string

let assert_ifd ~width ~height ?compression ?samples_per_pixel ?bits_per_sample
    ?sample_format ?rows_per_strip ?predictor ?planar_configuration ifd =
  assert_equal_int ~msg:"Image width" width (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" height (Tiff.Ifd.height ifd);
  (* Only check an optional property when an expected value was supplied. *)
  (match compression with
  | None -> ()
  | Some expected ->
      assert_equal ~msg:"Compression" expected (Tiff.Ifd.compression ifd));
  (match samples_per_pixel with
  | None -> ()
  | Some expected ->
      assert_equal_int ~msg:"Samples per pixel" expected
        (Tiff.Ifd.samples_per_pixel ifd));
  (match bits_per_sample with
  | None -> ()
  | Some expected ->
      assert_equal ~msg:"BPP" expected (Tiff.Ifd.bits_per_sample ifd));
  (match sample_format with
  | None -> ()
  | Some expected ->
      assert_equal ~msg:"sample format" expected (Tiff.Ifd.sample_format ifd));
  (match rows_per_strip with
  | None -> ()
  | Some expected ->
      assert_equal_int ~msg:"Rows per strip" expected
        (Tiff.Ifd.rows_per_strip ifd));
  (match predictor with
  | None -> ()
  | Some expected ->
      assert_equal ~msg:"Predictor" expected (Tiff.Ifd.predictor ifd));
  match planar_configuration with
  | None -> ()
  | Some expected ->
      assert_equal
        ~printer:(fun c ->
          Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
        ~msg:"Planar configuration" expected
        (Tiff.Ifd.planar_configuration ifd)
