(* TIFF file data is interpreted according to its specified byte order *)

type endianness = Big | Little

let uint16 ?(offset = 0) endian buf =
  match endian with
  | Big -> Cstruct.BE.get_uint16 buf offset
  | Little -> Cstruct.LE.get_uint16 buf offset

let int16 ?(offset = 0) endian buf =
  (uint16 ~offset endian buf lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let uint32 ?(offset = 0) endian buf =
  match endian with
  | Big -> Cstruct.BE.get_uint32 buf offset
  | Little -> Cstruct.LE.get_uint32 buf offset

let uint64 ?(offset = 0) endian buf =
  match endian with
  | Big -> Cstruct.BE.get_uint64 buf offset
  | Little -> Cstruct.LE.get_uint64 buf offset

let double ?(offset = 0) endian buf =
  Int64.float_of_bits (uint64 ~offset endian buf)
