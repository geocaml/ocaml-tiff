val decode : Cstruct.t -> Cstruct.t
val get_bits : Cstruct.t -> int -> int -> int
val flatten_codes : ?pad:bool -> int -> (Z.t * int) list -> Cstruct.t
