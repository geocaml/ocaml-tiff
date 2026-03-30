let ( / ) = Eio.Path.( / )

let checkerboard ~size =
  let v = Nx.zeros Nx.uint8 [| size; size |] in
  for row = 0 to size - 1 do
    for col = 0 to size - 1 do
      if (row + col) mod 2 = 0 then Nx.set_item [ row; col ] 254 v
    done
  done;
  Nx.to_bigarray v

let () =
  Eio_posix.run @@ fun env ->
  Tiff_eio.with_open_out (env#cwd / "example.tiff") @@ fun w ->
  let tif = Tiff.make (checkerboard ~size:256) in
  Tiff.to_file tif w
