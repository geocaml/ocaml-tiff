open Bechamel
open Toolkit

type test_case = E : ('a, 'b) Tiff.kind * string -> test_case

let tests =
  [
    E (Tiff.Uint8, "../testdata/uniform_uint8_lzw.tiff");
    E (Tiff.Uint8, "../testdata/striped_uint8_uncompressed.tiff");
    E (Tiff.Uint16, "../testdata/uniform_uint16_lzw.tiff");
    E (Tiff.Float32, "../testdata/uniform_float32_lzw.tiff");
  ]

let get_dims (E (kind, file)) =
  Staged.stage @@ fun () ->
  Tiff_unix.with_open_in file @@ fun ro ->
  let tiff = Tiff.from_file kind ro in
  let data = Tiff.data tiff ro in
  Sys.opaque_identity (ignore data)

let get_ifd (E (kind, file)) =
  Staged.stage @@ fun () ->
  Tiff_unix.with_open_in file @@ fun ro ->
  let tiff = Tiff.from_file kind ro in
  let ifd = Tiff.ifd tiff in
  let compression = Ifd.compression ifd in
  assert (compression = LZW || compression = No_compression);
  let width = Ifd.width ifd in
  Sys.opaque_identity (ignore (compression, width))

let read_data path =
  let size = (Unix.stat path).st_size in
  let buf = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout size in
  In_channel.with_open_bin path @@ fun ic ->
  let () =
    match In_channel.really_input_bigarray ic buf 0 size with
    | Some () -> ()
    | None -> assert false
  in
  Cstruct.of_bigarray buf

let lzw_bufs = [ ("cea", read_data "./corpus/raw_cea_lzw", 264_710) ]

let lzw (name, buf, expected_size) =
  Test.make ~name @@ Staged.stage
  @@ fun () ->
  let output = Cstruct.create expected_size in
  Tiff.Private.Lzw.decode buf output

let tests fn =
  List.map
    (fun (E (_, file) as e) ->
      Test.make ~name:(Filename.basename file |> Filename.chop_extension) (fn e))
    tests

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.5) ~kde:(Some 1000) ()
  in
  let read = Test.make_grouped ~name:"read" (tests get_dims) in
  let ifd = Test.make_grouped ~name:"ifd" (tests get_ifd) in
  let lzw = Test.make_grouped ~name:"lzw" (List.map lzw lzw_bufs) in
  let test = Test.make_grouped ~name:"tiff" [ lzw; ifd; read ] in
  let raw_results = Benchmark.all cfg instances test in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

let () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let () =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark () in
  img (window, results) |> eol |> output_image
