let checkerboard ~size =
  let v = Nx.zeros Nx.uint8 [| size; size |] in
  for row = 0 to size - 1 do
    for col = 0 to size - 1 do
      if (row + col) mod 2 = 0 then Nx.set_item [ row; col ] 254 v
    done
  done;
  v

let () =
  Eio_posix.run @@ fun env ->
  let tif = Eio.Path.(env#cwd / "example.tiff") in
  Tiff_eio.with_open_out tif @@ fun w ->
  let data = checkerboard ~size:256 |> Nx.to_bigarray in
  let tif = Tiff.make data in
  Tiff.to_file tif w;
  let ifd = Tiff.ifd tif in
  Eio.traceln "Tiff: %ix%i" (Tiff.Ifd.width ifd) (Tiff.Ifd.height ifd)
