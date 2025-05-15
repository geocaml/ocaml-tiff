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
  let compression = Tiff.Ifd.compression ifd in
  assert (compression = LZW || compression = No_compression);
  let width = Tiff.Ifd.width ifd in
  Sys.opaque_identity (ignore (compression, width))

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
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 1.5) ~kde:(Some 1000) ()
  in
  let read = Test.make_grouped ~name:"read" (tests get_dims) in
  let ifd = Test.make_grouped ~name:"ifd" (tests get_ifd) in
  let test = Test.make_grouped ~name:"tiff" [ ifd; read ] in
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
