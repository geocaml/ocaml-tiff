open Eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r = Path.(open_in ~sw (env#fs / Sys.argv.(1))) in
  let r = File.pread_exact r in
  let tiff = Tiff.from_file r in (* need to add data_type as param *)
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  let e = Tiff.Ifd.lookup_exn entries (Unknown 33550) in
  let bufs = Tiff.Ifd.read_entry_raw e r in
  let a = Int64.float_of_bits (Cstruct.LE.get_uint64 (List.hd bufs) 0) in
  let b =
    Int64.float_of_bits (Cstruct.LE.get_uint64 (List.hd (List.tl bufs)) 0)
  in
  Eio.traceln "%.20f, %.20f" a b