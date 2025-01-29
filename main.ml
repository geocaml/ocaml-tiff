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
  (* let e = Tiff.Ifd.lookup_exn entries (Unknown 33550) in
  let bufs = Tiff.Ifd.read_entry_raw e r in
  let a = Int64.float_of_bits (Cstruct.LE.get_uint64 (List.hd bufs) 0) in
  let b =
    Int64.float_of_bits (Cstruct.LE.get_uint64 (List.hd (List.tl bufs)) 0)
  in
  Eio.traceln "%.20f, %.20f" a b; *)

  (* E.g. when reading a UINT8 file: *)
  let data = Tiff.data tiff r Tiff.Data.UINT8 in
  let sum =
    match data with
    | UInt8Data data -> Owl.Dense.Ndarray.Generic.sum' data
    | _ -> raise Tiff.Data.TiffDataHasWrongType
  in

  Eio.traceln "Sum: %i" sum
