let () =
  Eio_posix.run @@ fun env ->
  let file = Eio.Path.(env#fs / Sys.argv.(1)) in
  Tiff_eio.with_open_in file @@ fun ro ->
  let tiff = Tiff.from_file Tiff.Int8 ro in
  tiff |> Tiff.ifd |> Tiff.Ifd.entries
  |> Fmt.pr "%a\n%!" Fmt.(list Tiff.Ifd.pp_entry);
  let ifd = Tiff.ifd tiff in
  Tiff.Ifd.bits_per_sample ifd |>
  Fmt.pr "bps: %a\n%!" Fmt.(list int)
