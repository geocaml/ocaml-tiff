val decode : Cstruct.t -> Cstruct.t -> unit
(** [decode src dst] inflates one zlib strip from [src] into [dst].

    [dst] must be exactly the uncompressed length of that strip. *)
