(* TIFF file data is interpreted according to its specified byte order *)

type endianness = Big | Little

let uint16 ?(offset = 0) endian buf =
  match endian with
  | Big -> Cstruct.BE.get_uint16 buf offset
  | Little -> Cstruct.LE.get_uint16 buf offset

let set_uint16 ?(offset = 0) endian buf magic =
  match endian with
  | Big -> Cstruct.BE.set_uint16 buf offset magic
  | Little -> Cstruct.LE.set_uint16 buf offset magic

let int16 ?(offset = 0) endian buf =
  (uint16 ~offset endian buf lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let uint32 ?(offset = 0) endian buf =
  match endian with
  | Big -> Cstruct.BE.get_uint32 buf offset
  | Little -> Cstruct.LE.get_uint32 buf offset

let set_uint32 ?(offset = 0) endian buf value =
  match endian with
  | Big -> Cstruct.BE.set_uint32 buf offset value
  | Little -> Cstruct.LE.set_uint32 buf offset value

let uint64 ?(offset = 0) endian buf =
  match endian with
  | Big -> Cstruct.BE.get_uint64 buf offset
  | Little -> Cstruct.LE.get_uint64 buf offset

let set_uint64 ?(offset = 0) endian buf value =
  match endian with
  | Big -> Cstruct.BE.set_uint64 buf offset value
  | Little -> Cstruct.LE.set_uint64 buf offset value

let double ?(offset = 0) endian buf =
  Int64.float_of_bits (uint64 ~offset endian buf)

let set_double ?(offset = 0) endian buf value =
  Int64.bits_of_float value |> set_uint64 ~offset endian buf
