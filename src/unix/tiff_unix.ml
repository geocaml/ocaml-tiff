(* Provides utilities for working with TIFF files *)

external preadv : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int
  = "caml_tiff_preadv"

external pwritev : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int
  = "caml_tiff_pwritev"

(* Retry short transfers. 0 bytes means no progress, so raise End_of_file. *)
let of_fd (fd : Unix.file_descr) : Tiff.File.ro =
  let rec read ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then
      match preadv fd (Array.of_list bufs) file_offset with
      | 0 -> raise End_of_file
      | n ->
          let file_offset =
            Optint.Int63.add file_offset (Optint.Int63.of_int n)
          in
          read ~file_offset (Cstruct.shiftv bufs n)
  in
  read

let of_fd_write (fd : Unix.file_descr) : Tiff.File.wo =
  let rec write ~file_offset bufs =
    if Cstruct.lenv bufs > 0 then
      match pwritev fd (Array.of_list bufs) file_offset with
      | 0 -> raise End_of_file
      | n ->
          let file_offset =
            Optint.Int63.add file_offset (Optint.Int63.of_int n)
          in
          write ~file_offset (Cstruct.shiftv bufs n)
  in
  write

let with_open_in ?(open_flags = [ Unix.O_RDONLY; Unix.O_CLOEXEC ]) s fn =
  let fd = Unix.openfile s open_flags 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> fn (of_fd fd))

let with_open_out ?(open_flags = [ Unix.O_WRONLY; Unix.O_CREAT; O_CLOEXEC ]) s
    fn =
  let fd = Unix.openfile s open_flags 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> fn (of_fd_write fd))
