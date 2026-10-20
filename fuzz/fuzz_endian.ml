(* Fuzz the endian read/write pairs: a written value must read back unchanged. *)

open Alcobar
module E = Tiff.Endian

let endian = choose [ const E.Big; const E.Little ]

let uint16_roundtrip e v =
  let buf = Cstruct.create 2 in
  E.set_uint16 e buf v;
  check_eq ~pp:pp_int (E.uint16 e buf) v

let int16_roundtrip e v =
  let buf = Cstruct.create 2 in
  E.set_int16 e buf v;
  check_eq ~pp:pp_int (E.int16 e buf) v

let uint32_roundtrip e v =
  let buf = Cstruct.create 4 in
  E.set_uint32 e buf v;
  check_eq ~pp:pp_int32 (E.uint32 e buf) v

let suite =
  ( "endian",
    [
      test_case "uint16 roundtrip" [ endian; uint16 ] uint16_roundtrip;
      test_case "int16 roundtrip" [ endian; int16 ] int16_roundtrip;
      test_case "uint32 roundtrip" [ endian; int32 ] uint32_roundtrip;
    ] )
