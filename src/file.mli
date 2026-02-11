type ro = file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(** Read-only access to a file that supports reading at a particular offset. The
    read should be exact and raise [End_of_file] should that be the case. *)

type wo = file_offset:Optint.Int63.t -> Cstruct.t list -> unit
(**Write-only access to a a file. The writing should be exact, that is, it
   should only return when all the bytes are written and raise [End_of_file]
   otherwise*)
