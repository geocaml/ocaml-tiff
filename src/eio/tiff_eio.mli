val with_open_in : _ Eio.Path.t -> (Tiff.File.ro -> 'a) -> 'a
(** [with_open_in path fn] opens [path] as a read-only file The {! Tiff.File.ro}
    is only valid for the scope of [fn] after which the underlying file will be
    closed. *)

val with_open_out : _ Eio.Path.t -> (Tiff.File.wo -> 'a) -> 'a
(** [with_open_in path fn] opens [path] as a write-only file. The file remains
    open until all the neccessary bytes are written to it*)
