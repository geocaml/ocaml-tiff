(* Unit tests for GeoTIFF file parsing and geospatial metadata extraction *)

open OUnit2

let cmp_float_list a b =
  List.fold_left2 (fun acc a b -> acc && cmp_float a b) true a b

let test_load_simple_uint8_tiff fs _ =
  Eio.Path.(with_open_in (fs / "./data/cea.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:514 ~height:515 ~samples_per_pixel:1
    ~bits_per_sample:[ 8 ] ~sample_format:Tiff.Ifd.UnsignedInteger header;
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
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Float32) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:7 ~height:3 ~samples_per_pixel:1
    ~bits_per_sample:[ 32 ] ~sample_format:Tiff.Ifd.IEEEFloatingPoint header;

  let pixel_scale = Array.to_list (Tiff.Ifd.pixel_scale header) in
  let expected = [ 0.016666666666667; 0.016666666666667; 0.0 ] in
  assert_bool "Pixel width" (cmp_float_list expected pixel_scale);

  let tie_point = Array.to_list (Tiff.Ifd.tiepoint header) in
  let expected = [ 0.; 0.; 0.; -73.566666666664560; 22.637233333845138; 0. ] in
  assert_bool "Pixel width" (cmp_float_list expected tie_point);

  assert_equal ~msg:"ascii params" [ "WGS 84" ]
    (Tiff.Ifd.geo_ascii_params header)

(* [pp_entry] prints "geokey: <name>, field: ...", and that name is the only
   view of the key the interface offers. *)
let key_name_of_entry e =
  let s = Format.asprintf "%a" Tiff.Ifd.GeoKeys.pp_entry e in
  let first = List.hd (String.split_on_char ',' s) in
  match String.index_opt first ' ' with
  | Some i -> String.sub first (i + 1) (String.length first - i - 1)
  | None -> first

let geokey_names fs path =
  Eio.Path.(with_open_in (fs / path)) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let header = Tiff.Ifd.read_header ro in
  let ifd = Tiff.Ifd.v ~file_offset:header.Tiff.Ifd.offset header ro in
  Tiff.Ifd.geo_key_directory ifd
  |> Tiff.Ifd.GeoKeys.get_geo_entries |> List.map key_name_of_entry

let test_projected_geokey_names fs _ =
  let expected =
    [
      "gt-model-type";
      "gt-raster-type";
      "gt-citation";
      "geographic-type";
      "geog-citation";
      "geog-angular-units";
      "projected-crs";
      "projection";
      "proj-coord-trans";
      "proj-linear-units";
      "proj-std-parallel-1";
      "proj-nat-origin-long";
      "proj-false-easting";
      "proj-false-northing";
    ]
  in
  assert_equal ~msg:"cea.tiff geokeys" ~printer:(String.concat ", ") expected
    (geokey_names fs "./data/cea.tiff")

let test_projected_geokey_names_with_ellipsoid fs _ =
  let expected =
    [
      "gt-model-type";
      "gt-raster-type";
      "gt-citation";
      "geographic-type";
      "geog-citation";
      "geog-angular-units";
      "geog-semi-major-axis";
      "geog-inv-flattening";
      "projected-crs";
      "projection";
      "proj-coord-trans";
      "proj-linear-units";
      "proj-std-parallel-1";
      "proj-nat-origin-long";
      "proj-false-easting";
      "proj-false-northing";
    ]
  in
  assert_equal ~msg:"uneven_rows_per_strip.tiff geokeys"
    ~printer:(String.concat ", ") expected
    (geokey_names fs "./data/uneven_rows_per_strip.tiff")

let test_geographic_geokey_names fs _ =
  let expected =
    [
      "gt-model-type";
      "gt-raster-type";
      "geographic-type";
      "geog-citation";
      "geog-angular-units";
      "geog-semi-major-axis";
      "geog-inv-flattening";
    ]
  in
  assert_equal ~msg:"aoh.tiff geokeys" ~printer:(String.concat ", ") expected
    (geokey_names fs "./data/aoh.tiff")

(* Every key in these files is one the GeoTIFF specification defines, so none
   of them should fall through to [Unknown]. *)
let test_no_unnamed_geokeys fs _ =
  List.iter
    (fun path ->
      List.iter
        (fun name ->
          assert_bool
            (Printf.sprintf "%s: key not named: %s" path name)
            (not (String.starts_with ~prefix:"unknown-" name)))
        (geokey_names fs path))
    [
      "./data/cea.tiff";
      "./data/cea_lzw.tiff";
      "./data/uneven_rows_per_strip.tiff";
      "./data/aoh.tiff";
    ]

let suite fs =
  "Basic tests"
  >::: [
         "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff fs;
         "Test load simple float32 geotiff"
         >:: test_load_simple_float32_geotiff fs;
         "Test projected geokey names" >:: test_projected_geokey_names fs;
         "Test projected geokey names with ellipsoid"
         >:: test_projected_geokey_names_with_ellipsoid fs;
         "Test geographic geokey names" >:: test_geographic_geokey_names fs;
         "Test no unnamed geokeys" >:: test_no_unnamed_geokeys fs;
       ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
