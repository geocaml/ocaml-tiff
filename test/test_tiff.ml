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
      Tiff_eio.with_open_out path fn
  | Unix -> Tiff_unix.with_open_out path fn

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
  let header = Tiff.Ifd.read_header r in
  let tiff = Tiff.from_file Tiff.Uint16 r in
  let ifd = Tiff.ifd tiff in
  let width = Tiff.Ifd.width ifd in
  let samples_per_pixel = Tiff.Ifd.samples_per_pixel ifd in
  let predictor = Tiff.Ifd.predictor ifd in

  let data = Tiff.data tiff r in
  Tiff.to_file ifd header tiff data w;

  let tiff = Tiff.from_file Tiff.Uint16 r2 in
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
  let header = Tiff.Ifd.read_header r in
  let tiff = Tiff.from_file Tiff.Uint8 r in
  let ifd = Tiff.ifd tiff in
  let width = Tiff.Ifd.width ifd in
  let samples_per_pixel = Tiff.Ifd.samples_per_pixel ifd in
  let predictor = Tiff.Ifd.predictor ifd in
  let data = Tiff.data tiff r in

  Tiff.to_file ifd header tiff data w;
  let tiff = Tiff.from_file Tiff.Uint8 r2 in
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
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
  let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 128) res

let test_load_data_as_wrong_type_fails backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Float32 ro in
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument
       "datatype not correct for plane: float32, Unsigned Integer, 8 bpp")
    (fun _ -> Tiff.data ~window tiff ro)

let test_read_single_plane_fails_when_specifying_plane backend _ =
  let data = "./data/uniform.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument "Can not select plane on single plane TIFFs") (fun _ ->
      Tiff.data ~plane:1 tiff ro)

let test_read_multi_plane_fails_when_without_specifying_plane backend _ =
  let data = "./data/uniform_rgb_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
  assert_raises ~msg:"fail to load data as wrong type"
    (Invalid_argument "Must specify plane for data read") (fun _ ->
      Tiff.data tiff ro)

let test_load_simple_int8_tiff _ =
  Tiff_unix.with_open_in "./data/uniform_int8_lzw.tiff" @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Int8 ro in
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
  let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
  (* uses alternating +ve and -ve values, so sum should be zero *)
  assert_equal_int ~msg:"Value sum" 0 res

let test_load_simple_uint8_tiff backend _ =
  let data = "./data/uniform_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
  let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 234) res

let test_load_simple_int16_tiff backend _ =
  let data = "./data/uniform_int16_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Int16 ro in
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
  let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 1234) res

let test_load_simple_uint16_tiff backend _ =
  let data = "./data/uniform_uint16_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint16 ro in
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
  let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
  assert_equal_int ~msg:"Value sum" (10 * 10 * 61234) res

let test_load_simple_int32_tiff backend _ =
  let data = "./data/uniform_int32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Int32 ro in
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
  let res = Nx.sum (Nx.cast Int32 data) |> Nx.item [] in
  (* sum of data is 0 as it is an equal mix of +ve and -ve values *)
  assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res

let test_load_simple_uint32_tiff backend _ =
  let data = "./data/uniform_uint32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint32 ro in
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
  assert_raises (Invalid_argument "Unsigned 32-bit coming soon...") (fun () ->
      let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
      let data = Tiff.data ~window tiff ro |> Nx.of_bigarray in
      let res = Nx.sum (Nx.cast Int32 data) |> Nx.item [] in
      assert_equal ~printer:Int32.to_string ~msg:"Value sum" Int32.zero res)

let test_load_simple_float32_tiff backend _ =
  let data = "./data/uniform_float32_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Float32 ro in
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
  let tiff = Tiff.from_file Tiff.Float64 ro in
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
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
    let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
    assert_equal_int
      ~msg:(Printf.sprintf "Check plane %d" plane)
      ((plane + 1) * 10 * 10)
      res
  done

let test_load_striped_uint8_uncompressed_tiff backend _ =
  let data = "./data/striped_uint8_uncompressed.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
    let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

let test_load_striped_uint8_lzw_tiff backend _ =
  let data = "./data/striped_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
    let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

let test_load_odd_striped_uint8_lzw_tiff backend _ =
  let data = "./data/striped_uint8_lzw_odd_strip_size.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
  let header = Tiff.ifd tiff in
  let width = Tiff.Ifd.width header in
  let height = Tiff.Ifd.height header in
  assert_equal_int ~msg:"Image width" 10 width;
  assert_equal_int ~msg:"Image height" 10 height;
  assert_equal_int ~msg:"Rows per strip" 3 (Tiff.Ifd.rows_per_strip header);
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
    let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
    assert_equal_int ~msg:"Value sum" ((y + 1) * width) res
  done

let test_uneven_rows_per_strip backend _ =
  let data = "./data/uneven_rows_per_strip.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
  let d = Tiff.data tiff ro |> Nx.of_bigarray in
  let res = Nx.sum (Nx.cast Int d) |> Nx.item [] in
  ignore res

let test_lzw_cea backend _ =
  let data = "./data/cea.tiff" in
  let data_compressed = "./data/cea_lzw.tiff" in
  let sum_data d =
    with_ro backend d @@ fun ro ->
    let tiff = Tiff.from_file Tiff.Uint8 ro in
    Tiff.data tiff ro |> Nx.of_bigarray |> Nx.sum |> Nx.item []
  in
  assert_equal_int ~msg:"sum" (sum_data data) (sum_data data_compressed)

let test_gdal_sparse_tiff backend _ =
  let data = "./data/sparse_uint8_lzw.tiff" in
  with_ro backend data @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
    let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
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
  let tiff = Tiff.from_file Tiff.Uint8 ro in
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
  let res = Nx.sum (Nx.cast Int data) |> Nx.item [] in
  (* The test image is filled with value 128, so sum should be 10*10*128 = 12800 *)
  assert_equal_int ~msg:"Value sum" (10 * 10 * 128) res

let suite fs =
  let tests backend =
    [
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
    ]
  in
  "Basic Tests" >::: [ "Eio" >::: tests (Eio fs); "Unix" >::: tests Unix ]

let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  run_test_tt_main (suite fs)
