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

  (* E.g. when reading a UINT8 file: *)
  let data = Tiff.data tiff r Tiff.Data.UINT8 in
  let sum =
    match data with
    | UInt8Data data -> Owl.Dense.Ndarray.Generic.sum' data
    | _ -> raise (Invalid_argument "Tiff Data has wrong type")
  in

  Eio.traceln "Sum: %i" sum
