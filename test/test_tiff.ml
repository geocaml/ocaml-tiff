(* End-to-end validation of TIFF file reading and decoding, checking both header interpretation and pixel data output *)

open OUnit2

type backend = Eio of Eio.Fs.dir_ty Eio.Path.t | Unix

let assert_equal_int = assert_equal ~printer:Int.to_string

let assert_equal_ba a b =
  let dims_a = Bigarray.Genarray.dims a in
  let dims_b = Bigarray.Genarray.dims b in
  assert_equal ~msg:"Dimensions"
    ~printer:(fun dims ->
      Array.to_list dims |> List.map string_of_int |> String.concat "x")
    dims_a dims_b;
  let size = Array.fold_left ( * ) 1 dims_a in
  let a = Bigarray.reshape_1 a size in
  let b = Bigarray.reshape_1 b size in
  for i = 0 to size - 1 do
    let va = Bigarray.Array1.get a i in
    let vb = Bigarray.Array1.get b i in
    if va <> vb then
      assert_failure
        (Printf.sprintf "Data mismatch at index %d: %d <> %d" i va vb)
  done

let with_ro backend path fn =
  match backend with
  | Eio fs ->
      let path = Eio.Path.(fs / path) in
      Tiff_eio.with_open_in path fn
  | Unix -> Tiff_unix.with_open_in path fn

let with_wo backend path fn =
  match backend with
  | Eio fs ->
      let path = Eio.Path.(fs / path) in
      let a = Tiff_eio.with_open_out path fn in
      Eio.Path.unlink path;
      a
  | Unix ->
      let a = Tiff_unix.with_open_out path fn in
      Unix.unlink path;
      a

let test_write_simple_striped_tiff backend _ =
  with_wo backend "./data/tmp_striped.tiff" @@ fun w ->
  with_ro backend "./data/tmp_striped.tiff" @@ fun r ->
  let data =
    Nx.init UInt16 [| 800; 20 |] (fun i -> Array.fold_left ( + ) 0 i)
    |> Nx.to_bigarray
  in
  let tiff = Tiff.make data in
  Tiff.to_file tiff w;
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint16) r in
  let _ifd = Tiff.ifd tiff in
  assert_equal ~msg:"Data" data (Tiff.data tiff r)

let test_write_basic_tiff backend _ =
  with_wo backend "./data/tmp.tiff" @@ fun w ->
  with_ro backend "./data/tmp.tiff" @@ fun r ->
  let data =
    Nx.init UInt8 [| 10; 10 |] (fun i -> Array.fold_left ( + ) 0 i)
    |> Nx.to_bigarray
  in
  let tiff = Tiff.make data in
  Tiff.to_file tiff w;
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) r in
  let ifd = Tiff.ifd tiff in
  let document_name = Tiff.Ifd.document_name ifd in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~bits_per_sample:[ 8 ] ifd;
  assert_equal ~msg:"Data" data (Tiff.data tiff r);
  assert_equal ~msg:"Document Name" document_name "TIFF_File"

let test_write_int32_tiff backend _ =
  with_wo backend "./data/tmp_int32.tiff" @@ fun w ->
  with_ro backend "./data/tmp_int32.tiff" @@ fun r ->
  let data =
    Bigarray.Genarray.create Bigarray.int32 Bigarray.c_layout [| 2; 2 |]
  in
  Bigarray.Genarray.fill data (-7l);
  let tiff = Tiff.make data in
  Tiff.to_file tiff w;
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Int32) r in
  assert_equal ~msg:"Sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format (Tiff.ifd tiff));
  assert_equal ~msg:"Data" data (Tiff.data tiff r)

let test_write_grayscale_photometric backend _ =
  with_wo backend "./data/tmp_gray.tiff" @@ fun w ->
  with_ro backend "./data/tmp_gray.tiff" @@ fun r ->
  let data =
    Bigarray.Genarray.create Bigarray.int8_unsigned Bigarray.c_layout [| 2; 2 |]
  in
  Bigarray.Genarray.fill data 1;
  Tiff.to_file (Tiff.make data) w;
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) r in
  let photometric =
    Tiff.Ifd.lookup_exn
      (Tiff.Ifd.entries (Tiff.ifd tiff))
      Tiff.Ifd.PhotometricInterpretation
    |> Tiff.Ifd.read_entry
  in
  assert_equal ~msg:"Photometric interpretation"
    (Tiff.Ifd.photometric_interpretation_to_int Tiff.Ifd.BlackIsZero)
    photometric

let test_normal_header_roundtrip backend _ =
  with_ro backend "./data/uniform.tiff" @@ fun r ->
  with_wo backend "./data/tmp.tiff" @@ fun w ->
  with_ro backend "./data/tmp.tiff" @@ fun r2 ->
  let header1 = Tiff.Ifd.read_header r in
  Tiff.Ifd.write_header w header1;
  let header2 = Tiff.Ifd.read_header r2 in
  assert_equal ~msg:"Equal headers" header1 header2

let test_bigtiff_header_roundtrip backend _ =
  with_ro backend "./data/color.tiff" @@ fun r ->
  with_wo backend "./data/tmp.tiff" @@ fun w ->
  with_ro backend "./data/tmp.tiff" @@ fun r2 ->
  let header1 = Tiff.Ifd.read_header r in
  Tiff.Ifd.write_header w header1;
  let header2 = Tiff.Ifd.read_header r2 in
  assert_equal ~msg:"Equal headers" header1 header2

let test_write_entries_roundtrip backend _ =
  with_ro backend "./data/u16bit.tiff" @@ fun r ->
  with_wo backend "./data/tmp.tiff" @@ fun w ->
  with_ro backend "./data/tmp.tiff" @@ fun r2 ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint16) r in
  let ifd = Tiff.ifd tiff in
  let width = Tiff.Ifd.width ifd in
  let samples_per_pixel = Tiff.Ifd.samples_per_pixel ifd in
  let predictor = Tiff.Ifd.predictor ifd in

  let data = Tiff.data tiff r in
  Tiff.to_file tiff w;

  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint16) r2 in
  let ifd = Tiff.ifd tiff in
  let data2 = Tiff.data tiff r2 in
  assert_equal ~msg:"Image widths" width (Tiff.Ifd.width ifd);
  assert_equal ~msg:"Samples per pixel" samples_per_pixel
    (Tiff.Ifd.samples_per_pixel ifd);
  assert_equal ~msg:"Predictor" predictor (Tiff.Ifd.predictor ifd);
  assert_equal ~msg:"Data" data data2

let test_bigtiff_write_entries_roundtrip backend _ =
  with_ro backend "./data/color.tiff" @@ fun r ->
  with_wo backend "./data/tmp.tiff" @@ fun w ->
  with_ro backend "./data/tmp.tiff" @@ fun r2 ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) r in
  let ifd = Tiff.ifd tiff in
  let width = Tiff.Ifd.width ifd in
  let samples_per_pixel = Tiff.Ifd.samples_per_pixel ifd in
  let predictor = Tiff.Ifd.predictor ifd in
  let data = Tiff.data tiff r in

  Tiff.to_file tiff w;
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) r2 in
  let ifd = Tiff.ifd tiff in
  let data2 = Tiff.data tiff r2 in
  assert_equal ~msg:"Image widths" width (Tiff.Ifd.width ifd);
  assert_equal ~msg:"Samples per pixel" samples_per_pixel
    (Tiff.Ifd.samples_per_pixel ifd);
  assert_equal ~msg:"Predictor" predictor (Tiff.Ifd.predictor ifd);
  assert_equal ~msg:"Data" data data2

let test_load_uniform_tiff backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:256 ~height:256
    ~compression:Tiff.Ifd.No_compression ~samples_per_pixel:1
    ~bits_per_sample:[ 8 ] ~predictor:Tiff.Ifd.No_predictor
    ~planar_configuration:Tiff.Ifd.Chunky header;
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 128) res

let test_load_data_as_wrong_type_fails backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Float32) ro in
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  assert_raises ~msg:"fail to load data as wrong type"
    (Tiff.Error.Tiff_error
       ( Tiff.Error.Invalid_argument,
         "datatype not correct for plane: float32, Unsigned Integer, 8 bpp",
         None ))
    (fun _ -> Tiff.data ~window tiff ro)

let test_read_single_plane_fails_when_specifying_plane backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  assert_raises
    (Tiff.Error.Tiff_error
       ( Tiff.Error.Invalid_argument,
         "Can not select plane on single plane TIFFs",
         None ))
    (fun () -> ignore (Tiff.data ~plane:1 tiff ro))

let test_read_multi_plane_fails_when_without_specifying_plane backend _ =
  let data = "./data/uniform_rgb_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  assert_raises ~msg:"fail to read multi-plane TIFF without specifying a plane"
    (Tiff.Error.Tiff_error
       ( Tiff.Error.Invalid_argument,
         "Cannot read planar TIFF data: a plane index must be specified",
         None ))
    (fun _ -> Tiff.data tiff ro)

let test_load_simple_int8_tiff backend _ =
  with_ro backend "./data/uniform_int8_lzw.tiff" @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:8 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 8 ]
    ~sample_format:Tiff.Ifd.SignedInteger ~predictor:Tiff.Ifd.No_predictor
    ~planar_configuration:Tiff.Ifd.Chunky header;
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:8 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  (* uses alternating +ve and -ve values, so sum should be zero *)
  assert_equal_int ~msg:"Value sum" 0 res

let test_load_simple_uint8_tiff backend _ =
  let data = "./data/uniform_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 8 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~predictor:Tiff.Ifd.No_predictor
    ~planar_configuration:Tiff.Ifd.Chunky header;
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 234) res

let test_load_simple_int16_tiff backend _ =
  let data = "./data/uniform_int16_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 16 ]
    ~sample_format:Tiff.Ifd.SignedInteger ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Chunky header;
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 1234) res

let test_load_simple_uint16_tiff backend _ =
  let data = "./data/uniform_uint16_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 16 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Chunky header;
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 61234) res

let test_load_simple_int32_tiff backend _ =
  let data = "./data/uniform_int32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int32) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 32 ]
    ~sample_format:Tiff.Ifd.SignedInteger ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Chunky header;
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] in
  (* sum of data is 0 as it is an equal mix of +ve and -ve values *)
  assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res

let test_load_simple_uint32_tiff backend _ =
  let data = "./data/uniform_uint32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint32) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 32 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Chunky header;
  assert_raises
    (Tiff.Error.Tiff_error
       ( Tiff.Error.Unsupported_feature,
         "Unsupported datatype for plane: uint32, Unsigned Integer, 32 bpp",
         None ))
    (fun () ->
      let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
      let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
      let res = Nx.sum (Nx.cast Int32 data) |> Nx.item [] in
      assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res)

let test_load_simple_float32_tiff backend _ =
  let data = "./data/uniform_float32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Float32) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 32 ]
    ~sample_format:Tiff.Ifd.IEEEFloatingPoint ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Chunky header;
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] in
  (* We need to force OCaml to get the 32 bit float representation of the number *)
  let value = Int32.float_of_bits (Int32.bits_of_float 1.2345) in
  let expected = ref 0. in
  for _ = 0 to 99 do
    expected := !expected +. value
  done;
  assert_bool "Value sum" (cmp_float !expected res)

let test_load_simple_float64_tiff backend _ =
  let data = "./data/uniform_float64_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Float64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 64 ]
    ~sample_format:Tiff.Ifd.IEEEFloatingPoint ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Chunky header;
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] in
  let expected = ref 0. in
  for _ = 0 to 99 do
    expected := !expected +. 1.2345
  done;
  assert_bool "Value sum" (cmp_float !expected res)

let uniform_rgb_uint8_lzw backend _ =
  let data = "./data/uniform_rgb_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:3 ~bits_per_sample:[ 8; 8; 8 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~rows_per_strip:10
    ~planar_configuration:Tiff.Ifd.Planar header;
  for plane = 0 to 2 do
    let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
    let data = Tiff.data ~plane ~window tiff ro |> Nx.of_bigarray in
    let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
    assert_equal_int
      ~msg:(Printf.sprintf "Check plane %d" plane)
      ((plane + 1) * 10 * 10)
      res
  done

let test_load_striped_uint8_uncompressed_tiff backend _ =
  let data = "./data/striped_uint8_uncompressed.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  let width = Tiff.Ifd.width header in
  let height = Tiff.Ifd.height header in
  Test_helpers.assert_ifd ~width:10 ~height:10
    ~compression:Tiff.Ifd.No_compression ~samples_per_pixel:1
    ~bits_per_sample:[ 8 ] ~sample_format:Tiff.Ifd.UnsignedInteger
    ~rows_per_strip:1 ~predictor:Tiff.Ifd.No_predictor
    ~planar_configuration:Tiff.Ifd.Chunky header;
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  for y = 0 to height - 1 do
    let window = Tiff.Window.make ~xoff:0 ~yoff:y ~xsize:width ~ysize:1 in
    let data = Tiff.data ~window tiff ro in
    let data2d = Bigarray.array2_of_genarray data in
    assert_equal_int ~msg:"data height" 1 (Bigarray.Array2.dim1 data2d);
    assert_equal_int ~msg:"data width" 10 (Bigarray.Array2.dim2 data2d);
    let data = Nx.of_bigarray data in
    let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

let test_load_striped_uint8_lzw_tiff backend _ =
  let data = "./data/striped_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  let width = Tiff.Ifd.width header in
  let height = Tiff.Ifd.height header in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 8 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~rows_per_strip:1
    ~predictor:Tiff.Ifd.No_predictor ~planar_configuration:Tiff.Ifd.Chunky
    header;
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  for y = 0 to height - 1 do
    let window = Tiff.Window.make ~xoff:0 ~yoff:y ~xsize:width ~ysize:1 in
    let data = Tiff.data ~window tiff ro in
    let data2d = Bigarray.array2_of_genarray data in
    assert_equal_int ~msg:"data height" 1 (Bigarray.Array2.dim1 data2d);
    assert_equal_int ~msg:"data width" 10 (Bigarray.Array2.dim2 data2d);
    let data = Nx.of_bigarray data in
    let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

let test_load_odd_striped_uint8_lzw_tiff backend _ =
  let data = "./data/striped_uint8_lzw_odd_strip_size.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  let width = Tiff.Ifd.width header in
  let height = Tiff.Ifd.height header in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 8 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~rows_per_strip:3
    ~predictor:Tiff.Ifd.No_predictor ~planar_configuration:Tiff.Ifd.Chunky
    header;
  assert_equal_int ~msg:"Number of strips" 4
    (List.length (Tiff.Ifd.data_offsets header));
  assert_equal ~msg:"Strip byte counts" [ 16; 16; 16; 7 ]
    (Tiff.Ifd.data_bytecounts header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  for y = 0 to height - 1 do
    let window = Tiff.Window.make ~xoff:0 ~yoff:y ~xsize:width ~ysize:1 in
    let data = Tiff.data ~window tiff ro in
    let data2d = Bigarray.array2_of_genarray data in
    assert_equal_int ~msg:"data height" 1 (Bigarray.Array2.dim1 data2d);
    assert_equal_int ~msg:"data width" 10 (Bigarray.Array2.dim2 data2d);
    let data = Nx.of_bigarray data in
    let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

let test_odd_striped_tiff_window_crosses_boundary backend _ =
  let path = "./data/striped_uint8_lzw_odd_strip_size.tiff" in
  with_ro backend path @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  let window = Tiff.Window.make ~xoff:0 ~yoff:2 ~xsize:10 ~ysize:3 in
  let data = Tiff.data ~window tiff ro in
  let data2d = Bigarray.array2_of_genarray data in
  assert_equal_int ~msg:"Window height" 3 (Bigarray.Array2.dim1 data2d);
  assert_equal_int ~msg:"Window width" 10 (Bigarray.Array2.dim2 data2d);
  let arr = Nx.of_bigarray data in
  let row_sums = Nx.(sum ~axes:[ 1 ] (cast Int64 arr)) in

  for row = 0 to 2 do
    let row_sum = Nx.item [ row ] row_sums |> Int64.to_int in
    assert_equal_int
      ~msg:(Printf.sprintf "Row %d sum" row)
      ((row + 3) * 10)
      row_sum
  done

let test_window_rejects_negative_values _ =
  let check ~msg ~xoff ~yoff ~xsize ~ysize =
    assert_raises ~msg
      (Tiff.Error.Tiff_error
         ( Tiff.Error.Invalid_argument,
           "TIFF window offsets and sizes must be non-negative",
           None ))
      (fun () -> ignore (Tiff.Window.make ~xoff ~yoff ~xsize ~ysize))
  in
  check ~msg:"Negative x offset" ~xoff:(-1) ~yoff:1 ~xsize:2 ~ysize:1;
  check ~msg:"Negative y offset" ~xoff:0 ~yoff:(-1) ~xsize:2 ~ysize:1;
  check ~msg:"Negative width" ~xoff:0 ~yoff:0 ~xsize:(-1) ~ysize:1;
  check ~msg:"Negative height" ~xoff:0 ~yoff:0 ~xsize:2 ~ysize:(-1)

let test_window_exceeds_image_dimensions backend _ =
  let path = "./data/striped_uint8_lzw_odd_strip_size.tiff" in
  with_ro backend path @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  let check window message =
    assert_raises
      (Tiff.Error.Tiff_error (Tiff.Error.Invalid_argument, message, None))
      (fun () -> ignore (Tiff.data ~window tiff ro))
  in
  check
    (Tiff.Window.make ~xoff:9 ~yoff:0 ~xsize:2 ~ysize:1)
    "Invalid TIFF data window { xoff=9, yoff=0, xsize=2, ysize=1 }: image \
     dimensions are 10 x 10";
  check
    (Tiff.Window.make ~xoff:0 ~yoff:9 ~xsize:1 ~ysize:2)
    "Invalid TIFF data window { xoff=0, yoff=9, xsize=1, ysize=2 }: image \
     dimensions are 10 x 10"

let test_window_at_image_boundary backend _ =
  let path = "./data/striped_uint8_lzw_odd_strip_size.tiff" in
  with_ro backend path @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  let window = Tiff.Window.make ~xoff:9 ~yoff:9 ~xsize:1 ~ysize:1 in
  let data = Tiff.data ~window tiff ro in
  let data2d = Bigarray.array2_of_genarray data in
  assert_equal_int ~msg:"Window height" 1 (Bigarray.Array2.dim1 data2d);
  assert_equal_int ~msg:"Window width" 1 (Bigarray.Array2.dim2 data2d);
  assert_equal_int ~msg:"Bottom-right pixel" 10 (Bigarray.Array2.get data2d 0 0)

let test_uneven_rows_per_strip backend _ =
  let data = "./data/uneven_rows_per_strip.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let d = Tiff.data tiff ro |> Nx.of_bigarray in
  let res = Nx.sum d |> Nx.item [] in
  ignore res

let test_lzw_cea backend _ =
  let data = "./data/cea.tiff" in
  let data_compressed = "./data/cea_lzw.tiff" in
  let sum_data d =
    with_ro backend d @@ fun ro ->
    let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
    Tiff.data tiff ro |> Nx.of_bigarray |> Nx.sum |> Nx.item []
  in
  assert_equal_int ~msg:"sum" (sum_data data) (sum_data data_compressed)

let check_predictor_matches backend kind plain_file predictor_file =
  let read_data ?(check_predictor = false) d =
    with_ro backend d @@ fun ro ->
    let tiff = Tiff.from_file (Tiff.as_kind kind) ro in
    if check_predictor then
      assert_equal ~msg:"Predictor" Tiff.Ifd.HorizontalDifferencing
        (Tiff.ifd tiff |> Tiff.Ifd.predictor);
    Tiff.data tiff ro
  in
  assert_equal_ba (read_data plain_file)
    (read_data ~check_predictor:true predictor_file)

let test_lzw_predictor_cea backend _ =
  check_predictor_matches backend Tiff.Uint8 "./data/cea.tiff"
    "./data/cea_lzw_predictor.tiff"

let test_lzw_predictor_rgb backend _ =
  check_predictor_matches backend Tiff.Uint8 "./data/normal.tiff"
    "./data/normal_rgb_lzw_predictor.tiff"

let test_lzw_predictor_u16 backend _ =
  check_predictor_matches backend Tiff.Uint16 "./data/u16bit.tiff"
    "./data/u16bit_rgb_lzw_predictor.tiff"

let test_lzw_predictor_u16_be backend _ =
  check_predictor_matches backend Tiff.Uint16 "./data/u16bit.tiff"
    "./data/u16bit_rgb_lzw_predictor_be.tiff"

let test_deflate_predictor_u16 backend _ =
  check_predictor_matches backend Tiff.Uint16 "./data/u16bit.tiff"
    "./data/u16bit_rgb_deflate_predictor.tiff"

let test_gdal_sparse_tiff backend _ =
  let data = "./data/sparse_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  let width = Tiff.Ifd.width header in
  let height = Tiff.Ifd.height header in
  Test_helpers.assert_ifd ~width:10 ~height:10 ~compression:Tiff.Ifd.LZW
    ~samples_per_pixel:1 ~bits_per_sample:[ 8 ]
    ~sample_format:Tiff.Ifd.UnsignedInteger ~rows_per_strip:1
    ~predictor:Tiff.Ifd.No_predictor ~planar_configuration:Tiff.Ifd.Chunky
    header;
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  for y = 0 to height - 1 do
    let window = Tiff.Window.make ~xoff:0 ~yoff:y ~xsize:width ~ysize:1 in
    let data = Tiff.data ~window tiff ro in
    let data2d = Bigarray.array2_of_genarray data in
    assert_equal_int ~msg:"data height" 1 (Bigarray.Array2.dim1 data2d);
    assert_equal_int ~msg:"data width" 10 (Bigarray.Array2.dim2 data2d);
    let data = Nx.of_bigarray data in
    let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
    assert_equal_int ~msg:"Value sum" ((y + 1) mod 2 * width) res
  done

let test_deflate_compression_types _ =
  (* Test that DEFLATE compression types exist *)
  assert_equal ~msg:"DEFLATE compression type" Tiff.Ifd.DEFLATE Tiff.Ifd.DEFLATE;
  assert_equal ~msg:"ADOBE_DEFLATE compression type" Tiff.Ifd.ADOBE_DEFLATE
    Tiff.Ifd.ADOBE_DEFLATE;

  (* Test that compression type strings are correct *)
  assert_equal ~msg:"DEFLATE string" "DEFLATE"
    (Tiff.Ifd.compression_to_string Tiff.Ifd.DEFLATE);
  assert_equal ~msg:"ADOBE_DEFLATE string" "ADOBE_DEFLATE"
    (Tiff.Ifd.compression_to_string Tiff.Ifd.ADOBE_DEFLATE);

  (* Test that Deflate module is accessible *)
  let _ = Tiff.Private.Deflate.decode in
  ()

let test_load_deflate_compressed_tiff backend _ =
  let data = "./data/test_deflate.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  Test_helpers.assert_ifd ~width:10 ~height:10
    ~compression:Tiff.Ifd.ADOBE_DEFLATE ~samples_per_pixel:1
    ~bits_per_sample:[ 8 ] ~sample_format:Tiff.Ifd.UnsignedInteger
    ~predictor:Tiff.Ifd.No_predictor ~planar_configuration:Tiff.Ifd.Chunky
    header;
  let window = Tiff.Window.make ~xoff:0 ~yoff:0 ~xsize:10 ~ysize:10 in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  (* The test image is filled with value 128, so sum should be 10*10*128 = 12800 *)
  assert_equal_int ~msg:"Value sum" (10 * 10 * 128) res

let test_load_multiple_uniform_ifds_tiff backend _ =
  let data = "./data/multiple_uniform_ifds.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint16) ro in
  let ifds = Tiff.ifds tiff in
  assert_equal_int ~msg:"Number of IFDs" 13 (Array.length ifds);
  (* stack all IFDs together, this would fail if their dimensions weren't
     uniform *)
  let data =
    Array.mapi
      (fun image_nb _ -> Tiff.data ~image_nb tiff ro |> Nx.of_bigarray)
      ifds
    |> Array.to_list |> Nx.stack
  in
  let shape = Nx.shape data in
  assert_equal ~printer:Nx.shape_to_string ~msg:"Aggregated data shape"
    [| 13; 314; 502 |] shape;
  OUnit2.assert_raises ~msg:"Request invalid image number"
    (Tiff.Error.Tiff_error
       ( Tiff.Error.Invalid_argument,
         "Image n°13 requested but TIFF file only has 13 ([0-12])",
         None ))
    (fun () -> Tiff.data ~image_nb:13 tiff ro)

let test_safe_cast_rejects_lossy backend _ =
  let rejects path intent msg =
    with_ro backend path @@ fun ro ->
    let tiff = Tiff.from_file intent ro in
    assert_raises
      (Tiff.Error.Tiff_error (Tiff.Error.Invalid_argument, msg, None))
      (fun () -> ignore (Tiff.data tiff ro))
  in
  rejects "./data/uniform.tiff" (Tiff.safe_cast Tiff.Int8)
    "safe cast to int8 would lose data: Unsigned Integer, 8 bpp";
  rejects "./data/uniform_uint32_lzw.tiff"
    (Tiff.safe_cast Tiff.Int32)
    "safe cast to int32 would lose data: Unsigned Integer, 32 bpp";
  rejects "./data/uniform_float64_lzw.tiff"
    (Tiff.safe_cast Tiff.Float32)
    "safe cast to float32 would lose data: IEEEFloatingPoint, 64 bpp"

let test_truncated_file_raises backend _ =
  let tmp = "./data/tmp_truncated.tiff" in
  let full =
    In_channel.with_open_bin "./data/uniform.tiff" In_channel.input_all
  in
  let truncate_to len =
    Out_channel.with_open_bin tmp (fun oc ->
        Out_channel.output_substring oc full 0 len)
  in
  Fun.protect
    ~finally:(fun () -> Sys.remove tmp)
    (fun () ->
      truncate_to 50;
      assert_raises ~msg:"IFD cut short" End_of_file (fun () ->
          with_ro backend tmp (fun ro ->
              Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro));
      truncate_to 30_000;
      with_ro backend tmp @@ fun ro ->
      let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
      assert_raises ~msg:"strip data cut short" End_of_file (fun () ->
          Tiff.data tiff ro))

let test_baseline_tags backend _ =
  let has_tag path tag =
    with_ro backend path @@ fun r ->
    let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) r in
    Option.is_some (Tiff.Ifd.lookup (Tiff.Ifd.entries (Tiff.ifd tiff)) tag)
  in
  assert_bool "Software" (has_tag "./data/color.tiff" Tiff.Ifd.Software);
  assert_bool "ImageDescription"
    (has_tag "./data/color.tiff" Tiff.Ifd.ImageDescription);
  assert_bool "ExtraSamples"
    (has_tag "./data/jello-gray.tiff" Tiff.Ifd.ExtraSamples)

let test_load_striped_no_rows_per_strip backend _ =
  let data = "./data/striped_no_rowsperstrip.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  (* The tag is missing, so it should default to the height (10) *)
  assert_equal_int ~msg:"Rows per strip defaults to height" 10
    (Tiff.Ifd.rows_per_strip header)

let test_malformed_tiff_header backend _ =
  let tmp = "./data/tmp_malformed_header.tiff" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists tmp then Sys.remove tmp)
    (fun () ->
      Out_channel.with_open_bin tmp (fun oc ->
          output_string oc "II";
          output_byte oc 0;
          output_byte oc 0;
          output_string oc "\000\000\000\000\000\000\000\000\000\000\000\000");
      with_ro backend tmp @@ fun ro ->
      match Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro with
      | exception Tiff.Error.Tiff_error (Tiff.Error.Malformed_file, _, None) ->
          ()
      | exception exn ->
          assert_failure ("Unexpected exception: " ^ Printexc.to_string exn)
      | _ -> assert_failure "Expected Malformed_file")

let test_missing_bits_per_sample backend _ =
  let data = "./data/1bit_no_bps.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal ~msg:"Bits per sample defaults to 1" [ 1 ]
    (Tiff.Ifd.bits_per_sample ifd)

let suite fs =
  let tests backend =
    [
      "Test write simple striped TIFF"
      >:: test_write_simple_striped_tiff backend;
      "Test write basic TIFF" >:: test_write_basic_tiff backend;
      "Test write int32 TIFF" >:: test_write_int32_tiff backend;
      "Test write ifd roundtrip" >:: test_write_entries_roundtrip backend;
      "Test odd striped TIFF window crosses boundary"
      >:: test_odd_striped_tiff_window_crosses_boundary backend;
      "Test window rejects negative values"
      >:: test_window_rejects_negative_values;
      "Test window exceeds image dimensions"
      >:: test_window_exceeds_image_dimensions backend;
      "Test window at image boundary" >:: test_window_at_image_boundary backend;
      "Test write bigtiff ifd roundtrip"
      >:: test_bigtiff_write_entries_roundtrip backend;
      "Test header roundtrip" >:: test_normal_header_roundtrip backend;
      "Test Bigtiff header roundtrip" >:: test_bigtiff_header_roundtrip backend;
      "Test DEFLATE compression types" >:: test_deflate_compression_types;
      "Test load DEFLATE compressed TIFF"
      >:: test_load_deflate_compressed_tiff backend;
      "Test load simple uniform uncompressed tiff"
      >:: test_load_uniform_tiff backend;
      "Test load data as wrong type"
      >:: test_load_data_as_wrong_type_fails backend;
      "Test fail to read plane from single layer tiff"
      >:: test_read_single_plane_fails_when_specifying_plane backend;
      "Test fail reading from multi-plane TIFF if plane not specified"
      >:: test_read_multi_plane_fails_when_without_specifying_plane backend;
      "Test load simple int8 tiff" >:: test_load_simple_int8_tiff backend;
      "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff backend;
      "Test load simple int16 tiff" >:: test_load_simple_int16_tiff backend;
      "Test load simple uint16 tiff" >:: test_load_simple_uint16_tiff backend;
      "Test load simple int32 tiff" >:: test_load_simple_int32_tiff backend;
      "Test load simple uint32 tiff" >:: test_load_simple_uint32_tiff backend;
      "Test write grayscale photometric"
      >:: test_write_grayscale_photometric backend;
      "Test safe cast rejects lossy conversion"
      >:: test_safe_cast_rejects_lossy backend;
      "Test load simple float32 tiff" >:: test_load_simple_float32_tiff backend;
      "Test load simple float64 tiff" >:: test_load_simple_float64_tiff backend;
      "Test load three channel tiff" >:: uniform_rgb_uint8_lzw backend;
      "Test load striped uint8 uncompressed tiff"
      >:: test_load_striped_uint8_uncompressed_tiff backend;
      "Test load striped uint8 lzw tiff"
      >:: test_load_striped_uint8_lzw_tiff backend;
      "Test load odd striped uint8 lzw tiff"
      >:: test_load_odd_striped_uint8_lzw_tiff backend;
      "Test uneven rows per strip" >:: test_uneven_rows_per_strip backend;
      "Test load striped tiff without RowsPerStrip"
      >:: test_load_striped_no_rows_per_strip backend;
      "Test LZW and NoCompress agree" >:: test_lzw_cea backend;
      "Test LZW predictor and NoCompress agree"
      >:: test_lzw_predictor_cea backend;
      "Test LZW predictor RGB and NoCompress agree"
      >:: test_lzw_predictor_rgb backend;
      "Test LZW predictor uint16 RGB and NoCompress agree"
      >:: test_lzw_predictor_u16 backend;
      "Test LZW predictor uint16 RGB big-endian and NoCompress agree"
      >:: test_lzw_predictor_u16_be backend;
      "Test Deflate predictor uint16 RGB and NoCompress agree"
      >:: test_deflate_predictor_u16 backend;
      "Test load GDAL sparse uint8 lzw tiff" >:: test_gdal_sparse_tiff backend;
      "Test load tiff with multiple uniform IFDs"
      >:: test_load_multiple_uniform_ifds_tiff backend;
      "Test truncated file raises End_of_file"
      >:: test_truncated_file_raises backend;
      "Test malformed TIFF header" >:: test_malformed_tiff_header backend;
      "Test missing BitsPerSample defaults to 1"
      >:: test_missing_bits_per_sample backend;
      "Test baseline tags" >:: test_baseline_tags backend;
    ]
  in
  "Basic Tests" >::: [ "Eio" >::: tests (Eio fs); "Unix" >::: tests Unix ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
