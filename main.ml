open Eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r = Path.(open_in ~sw (env#fs / Sys.argv.(1))) in
  let r = File.pread_exact r in
  let tiff = Tiff.from_file r in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "offsets: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_offsets ifd);
  Eio.traceln "counts: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_bytecounts ifd);
  (* E.g. when reading a UINT8 file: *)
  let data = Tiff.data tiff r Tiff.Data.Uint8 in
  let sum = Owl_base_dense_ndarray_generic.sum' data in
  Eio.traceln "Sum: %i" sum
