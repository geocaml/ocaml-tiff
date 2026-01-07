open Bigarray

val write_tiff : string -> (int, 'kind, c_layout) Genarray.t -> unit

val read_created_tiff :
  string -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Genarray.t
