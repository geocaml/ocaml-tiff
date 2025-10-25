(* Declares the LZW decompression function signature *)

val decode : Cstruct.t -> Cstruct.t -> unit
val get_bits : Cstruct.t -> int -> int -> int
