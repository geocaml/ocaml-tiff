let with_open_in path fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(with_open_in (fs / path)) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  fn ro
