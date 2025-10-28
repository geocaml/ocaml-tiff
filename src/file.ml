type ro = file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** Read-only access to a file that supports reading at a particular offset. The
    read should be exact and raise [End_of_file] should that be the case. *)
