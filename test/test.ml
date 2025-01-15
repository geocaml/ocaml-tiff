open Eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r = Path.(open_in ~sw (env#fs / Sys.argv.(1))) in
  let r = File.pread_exact r in
  let tiff = Tiff.from_file r (Tiff.Data.UINT8) in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "%ix%i" (Tiff.Ifd.height ifd) (Tiff.Ifd.width ifd);
  Eio.traceln "Samples per pixel: %i" (Tiff.Ifd.samples_per_pixel ifd);
  Eio.traceln "Bits per sample: %a"
    Fmt.(list int)
    (Tiff.Ifd.bits_per_sample ifd);
  Eio.traceln "offsets: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_offsets ifd);
  Eio.traceln "counts: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_bytecounts ifd)