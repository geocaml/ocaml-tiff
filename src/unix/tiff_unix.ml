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

(* Remove newly created files on write failure without deleting existing files. *)
let with_open_out
    ?(open_flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_CLOEXEC ]) s fn =
  let fd, created =
    if List.mem Unix.O_CREAT open_flags then
      try (Unix.openfile s (Unix.O_EXCL :: open_flags) 0o644, true)
      with Unix.Unix_error (Unix.EEXIST, _, _) ->
        (Unix.openfile s open_flags 0o644, false)
    else (Unix.openfile s open_flags 0o644, false)
  in
  try
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> fn (of_fd_write fd))
  with exn ->
    (if created then
       try Unix.unlink s with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
    raise exn
