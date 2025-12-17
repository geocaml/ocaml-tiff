(* Provides utitilities for working with TIFF files using the Eio library *)

let with_open_in fs path fn =
  Eio.Path.(with_open_in (fs / path)) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  fn ro
