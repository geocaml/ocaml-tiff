val of_fd : Unix.file_descr -> Tiff.File.ro
(** Creates a readable file abstraction from an {e open} file descriptor.

    The user is tasked with ensuring the file descriptor is kept open and valid.
*)

val with_open_in :
  ?open_flags:Unix.open_flag list -> string -> (Tiff.File.ro -> 'a) -> 'a
(** [with_open_in path fn] opens [path] as a read-only file and the
    close-on-exec flag set pass the {! Tiff.File.ro} to [fn]. The
    {! Tiff.File.ro} is only valid for the scope of [fn] after which the
    underlying file will be closed. *)
