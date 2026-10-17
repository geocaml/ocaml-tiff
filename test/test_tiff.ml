(* End-to-end validation of TIFF file reading and decoding, checking both header interpretation and pixel data output *)

open OUnit2

type backend = Eio of Eio.Fs.dir_ty Eio.Path.t | Unix

let assert_equal_int = assert_equal ~printer:Int.to_string

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
  let width = Tiff.Ifd.width ifd in
  let height = Tiff.Ifd.height ifd in
  assert_equal_int ~msg:"Image width" width 10;
  assert_equal_int ~msg:"Image Height" height 10;
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample ifd);
  assert_equal ~msg:"Data" data (Tiff.data tiff r);
  assert_equal ~msg:"Document Name" document_name "TIFF_File"

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
  assert_equal_int ~msg:"Image width" 256 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 256 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.No_compression
    (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 128) res

let test_load_data_as_wrong_type_fails backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Float32) ro in
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument
       "datatype not correct for plane: float32, Unsigned Integer, 8 bpp")
    (fun _ -> Tiff.data ~window tiff ro)

let test_read_single_plane_fails_when_specifying_plane backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument "Can not select plane on single plane TIFFs") (fun _ ->
      Tiff.data ~plane:1 tiff ro)

let test_read_multi_plane_fails_when_without_specifying_plane backend _ =
  let data = "./data/uniform_rgb_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint8) ro in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument "Must specify plane for data read") (fun _ ->
      Tiff.data tiff ro)

let test_load_simple_int8_tiff _ =
  Tiff_unix.with_open_in "./data/uniform_int8_lzw.tiff" @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 8 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 8; ysize = 10 } in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  (* uses alternating +ve and -ve values, so sum should be zero *)
  assert_equal_int ~msg:"Value sum" 0 res

let test_load_simple_uint8_tiff backend _ =
  let data = "./data/uniform_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 234) res

let test_load_simple_int16_tiff backend _ =
  let data = "./data/uniform_int16_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 16 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 1234) res

let test_load_simple_uint16_tiff backend _ =
  let data = "./data/uniform_uint16_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 16 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 61234) res

let test_load_simple_int32_tiff backend _ =
  let data = "./data/uniform_int32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int32) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 32 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.SignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
  let res = Nx.sum data |> Nx.item [] in
  (* sum of data is 0 as it is an equal mix of +ve and -ve values *)
  assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res

let test_load_simple_uint32_tiff backend _ =
  let data = "./data/uniform_uint32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Uint32) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 32 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  assert_raises
    (Invalid_argument
       "Unsupported datatype for plane: uint32, Unsigned Integer, 32 bpp")
    (fun () ->
      let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
      let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
      let res = Nx.sum (Nx.cast Int32 data) |> Nx.item [] in
      assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res)

let test_load_simple_float32_tiff backend _ =
  let data = "./data/uniform_float32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.as_kind Tiff.Float32) ro in
  let header = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 32 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.IEEEFloatingPoint
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
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
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 64 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.IEEEFloatingPoint
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
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
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 3
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8; 8; 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal_int ~msg:"Rows per strip" 10 (Tiff.Ifd.rows_per_strip header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Planar
    (Tiff.Ifd.planar_configuration header);
  for plane = 0 to 2 do
    let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
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
  assert_equal_int ~msg:"Image width" 10 width;
  assert_equal_int ~msg:"Image height" 10 height;
  assert_equal_int ~msg:"Rows per strip" 1 (Tiff.Ifd.rows_per_strip header);
  assert_equal ~msg:"Compression" Tiff.Ifd.No_compression
    (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  for y = 0 to height - 1 do
    let window = Tiff.{ xoff = 0; yoff = y; xsize = width; ysize = 1 } in
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
  assert_equal_int ~msg:"Image width" 10 width;
  assert_equal_int ~msg:"Image height" 10 height;
  assert_equal_int ~msg:"Rows per strip" 1 (Tiff.Ifd.rows_per_strip header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  for y = 0 to height - 1 do
    let window = Tiff.{ xoff = 0; yoff = y; xsize = width; ysize = 1 } in
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
  assert_equal_int ~msg:"Image width" 10 width;
  assert_equal_int ~msg:"Image height" 10 height;
  assert_equal_int ~msg:"Rows per strip" 3 (Tiff.Ifd.rows_per_strip header);
  assert_equal_int ~msg:"Number of strips" 4
    (List.length (Tiff.Ifd.strip_offsets header));
  assert_equal ~msg:"Strip byte counts" [ 16; 16; 16; 7 ]
    (Tiff.Ifd.strip_byte_counts header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  for y = 0 to height - 1 do
    let window = Tiff.{ xoff = 0; yoff = y; xsize = width; ysize = 1 } in
    let data = Tiff.data ~window tiff ro in
    let data2d = Bigarray.array2_of_genarray data in
    assert_equal_int ~msg:"data height" 1 (Bigarray.Array2.dim1 data2d);
    assert_equal_int ~msg:"data width" 10 (Bigarray.Array2.dim2 data2d);
    let data = Nx.of_bigarray data in
    let res = Nx.sum data |> Nx.item [] |> Int64.to_int in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

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

let test_gdal_sparse_tiff backend _ =
  let data = "./data/sparse_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let header = Tiff.ifd tiff in
  let width = Tiff.Ifd.width header in
  let height = Tiff.Ifd.height header in
  assert_equal_int ~msg:"Image width" 10 width;
  assert_equal_int ~msg:"Image height" 10 height;
  assert_equal_int ~msg:"Rows per strip" 1 (Tiff.Ifd.rows_per_strip header);
  assert_equal ~msg:"Compression" Tiff.Ifd.LZW (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_raises ~msg:"Pixel width" Not_found (fun () ->
      Tiff.Ifd.pixel_scale header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  for y = 0 to height - 1 do
    let window = Tiff.{ xoff = 0; yoff = y; xsize = width; ysize = 1 } in
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
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width header);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height header);
  assert_equal ~msg:"Compression" Tiff.Ifd.ADOBE_DEFLATE
    (Tiff.Ifd.compression header);
  assert_equal_int ~msg:"Samples per pixel" 1
    (Tiff.Ifd.samples_per_pixel header);
  assert_equal ~msg:"BPP" [ 8 ] (Tiff.Ifd.bits_per_sample header);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format header);
  assert_equal ~msg:"Predictor" Tiff.Ifd.No_predictor
    (Tiff.Ifd.predictor header);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Chunky
    (Tiff.Ifd.planar_configuration header);
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
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
    (Invalid_argument "Image n°13 requested but TIFF file only has 13 ([0-12])")
    (fun () -> Tiff.data ~image_nb:13 tiff ro)

let test_safe_cast_rejects_lossy backend _ =
  let rejects path intent msg =
    with_ro backend path @@ fun ro ->
    let tiff = Tiff.from_file intent ro in
    assert_raises (Invalid_argument msg) (fun () -> ignore (Tiff.data tiff ro))
  in
  rejects "./data/uniform.tiff" (Tiff.safe_cast Tiff.Int8)
    "safe cast to int8 would lose data: Unsigned Integer, 8 bpp";
  rejects "./data/uniform_uint32_lzw.tiff"
    (Tiff.safe_cast Tiff.Int32)
    "safe cast to int32 would lose data: Unsigned Integer, 32 bpp";
  rejects "./data/uniform_float64_lzw.tiff"
    (Tiff.safe_cast Tiff.Float32)
    "safe cast to float32 would lose data: IEEEFloatingPoint, 64 bpp"

(* Tiled TIFFs (issue #34): synthetic data where every pixel is a formula of
   its coordinates, so tests recompute expected values. *)

let assert_equal_int64 = assert_equal ~printer:Int64.to_string

let assert_equal_int64s =
  assert_equal ~printer:(fun l ->
      String.concat ";" (List.map Int64.to_string l))

let pixel2 data ~x ~y = Bigarray.Genarray.get data [| y; x |]
let pixel3 data ~x ~y ~c = Bigarray.Genarray.get data [| y; x; c |]

let check_pixels2 data ~width ~height value =
  let d = Bigarray.array2_of_genarray data in
  assert_equal_int ~msg:"Data height" height (Bigarray.Array2.dim1 d);
  assert_equal_int ~msg:"Data width" width (Bigarray.Array2.dim2 d);
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      assert_equal_int64
        ~msg:(Printf.sprintf "pixel (%d,%d)" x y)
        (Int64.of_int (value x y))
        (pixel2 data ~x ~y)
    done
  done

let check_pixels3 data ~width ~height ~channels value =
  let d = Bigarray.array3_of_genarray data in
  assert_equal_int ~msg:"Data height" height (Bigarray.Array3.dim1 d);
  assert_equal_int ~msg:"Data width" width (Bigarray.Array3.dim2 d);
  assert_equal_int ~msg:"Data channels" channels (Bigarray.Array3.dim3 d);
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      for c = 0 to channels - 1 do
        assert_equal_int64
          ~msg:(Printf.sprintf "pixel (%d,%d) channel %d" x y c)
          (Int64.of_int (value x y c))
          (pixel3 data ~x ~y ~c)
      done
    done
  done

let test_load_tiled_uint16_pattern backend _ =
  let data = "./data/tiled_uint16_pattern.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 45 (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" 50 (Tiff.Ifd.height ifd);
  assert_equal_int ~msg:"Tile width" 16 (Tiff.Ifd.tile_width ifd);
  assert_equal_int ~msg:"Tile height" 16 (Tiff.Ifd.tile_height ifd);
  assert_equal_int ~msg:"Tiles across" 3 (Tiff.Ifd.tiles_across ifd);
  assert_equal_int ~msg:"Tiles down" 4 (Tiff.Ifd.tiles_down ifd);
  assert_equal_int ~msg:"Number of tiles" 12
    (List.length (Tiff.Ifd.tile_offsets ifd));
  assert_equal_int64s ~msg:"Tile byte counts"
    (List.init 12 (fun _ -> 512L))
    (Tiff.Ifd.tile_byte_counts ifd);
  assert_equal ~msg:"BPP" [ 16 ] (Tiff.Ifd.bits_per_sample ifd);
  assert_equal ~printer:Tiff.Ifd.compression_to_string ~msg:"Compression"
    Tiff.Ifd.No_compression (Tiff.Ifd.compression ifd);
  let value x y = (y * 45) + x in
  let arr = Tiff.data tiff ro in
  check_pixels2 arr ~width:45 ~height:50 value;
  (* window crossing a tile boundary *)
  let window = Tiff.{ xoff = 14; yoff = 14; xsize = 6; ysize = 6 } in
  let win = Tiff.data ~window tiff ro in
  check_pixels2 win ~width:6 ~height:6 (fun x y -> value (x + 14) (y + 14))

let test_load_tiled_uint8_deflate backend _ =
  let data = "./data/tiled_uint8_deflate.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 45 (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" 50 (Tiff.Ifd.height ifd);
  assert_equal_int ~msg:"Tile width" 16 (Tiff.Ifd.tile_width ifd);
  assert_equal_int ~msg:"Tile height" 16 (Tiff.Ifd.tile_height ifd);
  assert_equal_int ~msg:"Tiles across" 3 (Tiff.Ifd.tiles_across ifd);
  assert_equal_int ~msg:"Tiles down" 4 (Tiff.Ifd.tiles_down ifd);
  assert_equal_int ~msg:"Number of tiles" 12
    (List.length (Tiff.Ifd.tile_offsets ifd));
  assert_equal ~printer:Tiff.Ifd.compression_to_string ~msg:"Compression"
    Tiff.Ifd.ADOBE_DEFLATE (Tiff.Ifd.compression ifd);
  let value x y = ((y * 45) + x) mod 256 in
  let arr = Tiff.data tiff ro in
  check_pixels2 arr ~width:45 ~height:50 value;
  let window = Tiff.{ xoff = 14; yoff = 14; xsize = 6; ysize = 6 } in
  let win = Tiff.data ~window tiff ro in
  check_pixels2 win ~width:6 ~height:6 (fun x y -> value (x + 14) (y + 14))

let test_load_tiled_rgb backend _ =
  let data = "./data/tiled_rgb_uint8.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 45 (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" 50 (Tiff.Ifd.height ifd);
  assert_equal_int ~msg:"Tile width" 16 (Tiff.Ifd.tile_width ifd);
  assert_equal_int ~msg:"Tile height" 16 (Tiff.Ifd.tile_height ifd);
  assert_equal_int ~msg:"Tiles across" 3 (Tiff.Ifd.tiles_across ifd);
  assert_equal_int ~msg:"Tiles down" 4 (Tiff.Ifd.tiles_down ifd);
  assert_equal_int ~msg:"Samples per pixel" 3 (Tiff.Ifd.samples_per_pixel ifd);
  assert_equal ~msg:"BPP" [ 8; 8; 8 ] (Tiff.Ifd.bits_per_sample ifd);
  assert_equal ~msg:"sample format" Tiff.Ifd.UnsignedInteger
    (Tiff.Ifd.sample_format ifd);
  let value x y c = match c with 0 -> x | 1 -> y | _ -> (x + y) mod 256 in
  let arr = Tiff.data tiff ro in
  check_pixels3 arr ~width:45 ~height:50 ~channels:3 value;
  let window = Tiff.{ xoff = 14; yoff = 14; xsize = 6; ysize = 6 } in
  let win = Tiff.data ~window tiff ro in
  check_pixels3 win ~width:6 ~height:6 ~channels:3 (fun x y c ->
      value (x + 14) (y + 14) c)

let test_load_planar_tiled backend _ =
  let data = "./data/tiled_planar_rgb.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 45 (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" 50 (Tiff.Ifd.height ifd);
  assert_equal_int ~msg:"Tile width" 16 (Tiff.Ifd.tile_width ifd);
  assert_equal_int ~msg:"Tile height" 16 (Tiff.Ifd.tile_height ifd);
  assert_equal_int ~msg:"Tiles across" 3 (Tiff.Ifd.tiles_across ifd);
  assert_equal_int ~msg:"Tiles down" 4 (Tiff.Ifd.tiles_down ifd);
  assert_equal
    ~printer:(fun c -> Int.to_string (Tiff.Ifd.planar_configuration_to_int c))
    ~msg:"Planar configuration" Tiff.Ifd.Planar
    (Tiff.Ifd.planar_configuration ifd);
  assert_raises ~msg:"Plane must be specified for planar data"
    (Invalid_argument "Must specify plane for data read") (fun () ->
      ignore (Tiff.data tiff ro));
  let value plane x y = (x + y + (10 * plane)) mod 256 in
  for plane = 0 to 2 do
    let arr = Tiff.data ~plane tiff ro in
    check_pixels2 arr ~width:45 ~height:50 (value plane)
  done;
  let window = Tiff.{ xoff = 14; yoff = 14; xsize = 6; ysize = 6 } in
  let win = Tiff.data ~plane:1 ~window tiff ro in
  check_pixels2 win ~width:6 ~height:6 (fun x y -> value 1 (x + 14) (y + 14))

let test_load_tiled_sparse backend _ =
  let data = "./data/tiled_sparse.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 40 (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" 40 (Tiff.Ifd.height ifd);
  assert_equal_int ~msg:"Tile width" 16 (Tiff.Ifd.tile_width ifd);
  assert_equal_int ~msg:"Tile height" 16 (Tiff.Ifd.tile_height ifd);
  assert_equal_int ~msg:"Tiles across" 3 (Tiff.Ifd.tiles_across ifd);
  assert_equal_int ~msg:"Tiles down" 3 (Tiff.Ifd.tiles_down ifd);
  assert_equal_int64s ~msg:"Tile byte counts (tiles 1,2,3,5,6,7 are empty)"
    [ 256L; 0L; 0L; 0L; 256L; 0L; 0L; 0L; 256L ]
    (Tiff.Ifd.tile_byte_counts ifd);
  let offsets = Tiff.Ifd.tile_offsets ifd in
  assert_bool "empty tiles 2, 5, 6 carry non-zero offsets"
    (List.for_all
       (fun t -> not (Int64.equal (List.nth offsets t) 0L))
       [ 2; 5; 6 ]);
  assert_bool "empty tiles 1, 3, 7 carry zero offsets"
    (List.for_all (fun t -> Int64.equal (List.nth offsets t) 0L) [ 1; 3; 7 ]);
  let value x y =
    let t = (y / 16 * 3) + (x / 16) in
    if List.mem t [ 0; 4; 8 ] then ((y * 40) + x) mod 256 else 0
  in
  let arr = Tiff.data tiff ro in
  check_pixels2 arr ~width:40 ~height:40 value;
  let window = Tiff.{ xoff = 8; yoff = 8; xsize = 26; ysize = 26 } in
  let win = Tiff.data ~window tiff ro in
  check_pixels2 win ~width:26 ~height:26 (fun x y -> value (x + 8) (y + 8))

let test_load_tiled_single_tile backend _ =
  let data = "./data/tiled_single_tile.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file (Tiff.safe_cast Tiff.Int64) ro in
  let ifd = Tiff.ifd tiff in
  assert_equal_int ~msg:"Image width" 10 (Tiff.Ifd.width ifd);
  assert_equal_int ~msg:"Image height" 10 (Tiff.Ifd.height ifd);
  assert_equal_int ~msg:"Tile width" 16 (Tiff.Ifd.tile_width ifd);
  assert_equal_int ~msg:"Tile height" 16 (Tiff.Ifd.tile_height ifd);
  assert_equal_int ~msg:"Tiles across" 1 (Tiff.Ifd.tiles_across ifd);
  assert_equal_int ~msg:"Tiles down" 1 (Tiff.Ifd.tiles_down ifd);
  (* 256 = full 16x16 tile; offset and count fit inline *)
  assert_equal_int64s ~msg:"Tile byte counts" [ 256L ]
    (Tiff.Ifd.tile_byte_counts ifd);
  assert_equal_int ~msg:"Number of tiles" 1
    (List.length (Tiff.Ifd.tile_offsets ifd));
  let arr = Tiff.data tiff ro in
  check_pixels2 arr ~width:10 ~height:10 (fun x y -> (y * 10) + x)

let suite fs =
  let tests backend =
    [
      "Test write simple striped TIFF"
      >:: test_write_simple_striped_tiff backend;
      "Test write basic TIFF" >:: test_write_basic_tiff backend;
      "Test write ifd roundtrip" >:: test_write_entries_roundtrip backend;
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
      "Test load simple int8 tiff" >:: test_load_simple_int8_tiff;
      "Test load simple uint8 tiff" >:: test_load_simple_uint8_tiff backend;
      "Test load simple int16 tiff" >:: test_load_simple_int16_tiff backend;
      "Test load simple uint16 tiff" >:: test_load_simple_uint16_tiff backend;
      "Test load simple int32 tiff" >:: test_load_simple_int32_tiff backend;
      "Test load simple uint32 tiff" >:: test_load_simple_uint32_tiff backend;
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
      "Test LZW and NoCompress agree" >:: test_lzw_cea backend;
      "Test load GDAL sparse uint8 lzw tiff" >:: test_gdal_sparse_tiff backend;
      "Test load tiff with multiple uniform IFDs"
      >:: test_load_multiple_uniform_ifds_tiff backend;
      "Test load tiled uint16 pattern"
      >:: test_load_tiled_uint16_pattern backend;
      "Test load tiled deflate tiff" >:: test_load_tiled_uint8_deflate backend;
      "Test load tiled rgb tiff" >:: test_load_tiled_rgb backend;
      "Test load planar tiled tiff" >:: test_load_planar_tiled backend;
      "Test load tiled sparse tiff" >:: test_load_tiled_sparse backend;
      "Test load tiled single tile tiff" >:: test_load_tiled_single_tile backend;
    ]
  in
  "Basic Tests" >::: [ "Eio" >::: tests (Eio fs); "Unix" >::: tests Unix ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
