open Eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r = Path.(open_in ~sw (env#fs / "test/cea.tiff")) in
  let tiff = Tiff.of_file (r :> File.ro) in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "%ix%i"
    (Tiff.Ifd.height (Tiff.ifd tiff))
    (Tiff.Ifd.width (Tiff.ifd tiff));
  Eio.traceln "offsets: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_offsets ifd);
  Eio.traceln "counts: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_bytecounts ifd)
