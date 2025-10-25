(* Provides utilities for working with TIFF files using Unix file descriptors *)

external preadv : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int
  = "caml_tiff_preadv"

let of_fd (fd : Unix.file_descr) : Tiff.File.ro =
 fun ~file_offset bufs ->
  let _ : int = preadv fd (Array.of_list bufs) file_offset in
  ()

let with_open_in ?(open_flags = [ Unix.O_RDONLY; Unix.O_CLOEXEC ]) s fn =
  let fd = Unix.openfile s open_flags 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> fn (of_fd fd))
