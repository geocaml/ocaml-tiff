val with_open_in : _ Eio.Path.t -> string -> (Tiff.File.ro -> 'a) -> 'a
(** [with_open_in fs path fn] opens [fs/path] as a read-only file The
    {! Tiff.File.ro} is only valid for the scope of [fn] after which the
    underlying file will be closed. *)
