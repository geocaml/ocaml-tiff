let () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(with_open_in (fs / "./testdata/uniform.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file Tiff.Uint8 ro in
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  Eio.traceln "We expect %i and got %i" (10 * 10 * 128) res
