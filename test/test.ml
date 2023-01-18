open Eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r = Path.(open_in ~sw (env#fs / "test/cea.tiff")) in
  let tiff = Tiff.of_file (r :> File.ro) in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) tiff.ifd.entries;
  Eio.traceln "%ldx%ld" (Tiff.Ifd.height tiff.ifd.entries).offset (Tiff.Ifd.width tiff.ifd.entries).offset;
  Eio.traceln "offsets: %a" Fmt.(list ~sep:(any ", ") int) tiff.ifd.data_offsets;
  Eio.traceln "counts: %a" Fmt.(list ~sep:(any ", ") int) tiff.ifd.data_bytecounts