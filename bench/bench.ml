open Bechamel
open Toolkit

type test_case = E : ('a, 'b) Tiff.kind * string -> test_case
type backend = Eio of Eio.Fs.dir_ty Eio.Path.t | Unix

let with_ro backend file fn =
  match backend with
  | Eio fs ->
      let path = Eio.Path.(fs / file) in
      Tiff_eio.with_open_in path fn
  | Unix -> Tiff_unix.with_open_in file fn

let tests =
  [
    E (Tiff.Uint8, "../test/data/uniform_uint8_lzw.tiff");
    E (Tiff.Uint8, "../test/data/striped_uint8_uncompressed.tiff");
    E (Tiff.Uint16, "../test/data/uniform_uint16_lzw.tiff");
    E (Tiff.Float32, "../test/data/uniform_float32_lzw.tiff");
  ]

let get_dims backend (E (kind, file)) =
  Staged.stage @@ fun () ->
  with_ro backend file @@ fun ro ->
  let tiff = Tiff.from_file kind ro in
  let data = Tiff.data tiff ro in
  Sys.opaque_identity (ignore data)

let get_ifd backend (E (kind, file)) =
  Staged.stage @@ fun () ->
  with_ro backend file @@ fun ro ->
  let tiff = Tiff.from_file kind ro in
  let ifd = Tiff.ifd tiff in
  let compression = Tiff.Ifd.compression ifd in
  assert (compression = LZW || compression = No_compression);
  let width = Tiff.Ifd.width ifd in
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

let tests fn backend =
  List.map
    (fun (E (_, file) as e) ->
      Test.make
        ~name:(Filename.basename file |> Filename.chop_extension)
        (fn backend e))
    tests

let benchmark fs () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let instances =
    Instance.[ minor_allocated; major_allocated; monotonic_clock ]
  in
  let cfg =
    Benchmark.cfg ~limit:3000 ~quota:(Time.second 2.5) ~kde:(Some 1000) ()
  in

  let backends = [ ("Unix", Unix); ("Eio", Eio fs) ] in

  let backend_tests =
    List.map
      (fun (name, backend) ->
        let read = Test.make_grouped ~name:"read" (tests get_dims backend) in
        let ifd = Test.make_grouped ~name:"ifd" (tests get_ifd backend) in
        Test.make_grouped ~name:("tiff_" ^ name) [ read; ifd ])
      backends
  in
  let lzw = Test.make_grouped ~name:"lzw" (List.map lzw lzw_bufs) in
  let test = Test.make_grouped ~name:"tiff" (backend_tests @ [ lzw ]) in

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
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results, _ = benchmark fs () in
  img (window, results) |> eol |> output_image
