val with_open_in : _ Eio.Path.t -> (Tiff.File.ro -> 'a) -> 'a
(** [with_open_in path fn] opens [path] as a read-only file The {! Tiff.File.ro}
    is only valid for the scope of [fn] after which the underlying file will be
    closed. *)
