open Eio

type header = {
  byte_order : endianness;
  offset : Optint.Int63.t;
}

and endianness = Big | Little

module Endian = struct
  let uint16 ?(offset=0) endian buf = match endian with
    | Big -> Cstruct.BE.get_uint16 buf offset
    | Little -> Cstruct.LE.get_uint16 buf offset

  let uint32 ?(offset=0) endian buf = match endian with
    | Big -> Cstruct.BE.get_uint32 buf offset
    | Little -> Cstruct.LE.get_uint32 buf offset
end

let pp_byte_order ppf = function
  | Big -> Fmt.pf ppf "Big-endian"
  | Little -> Fmt.pf ppf "Little-endian"

let header ro =
  let buf = Cstruct.create 8 in
  File.pread_exact ro ~file_offset:Optint.Int63.zero [ buf ];
  let byte_order =
    match Cstruct.to_string ~off:0 ~len:2 buf with
    | "II" -> Little
    | "MM" -> Big
    | e -> failwith ("Unknown endianness of TIFF file " ^ e)
  in
  let magic = Endian.uint16 ~offset:2 byte_order buf in
  assert (magic = 42);
  let offset = Endian.uint32 ~offset:4 byte_order buf |> Optint.Int63.of_int32 in
  { byte_order; offset }

module Ifd = struct
  type field = Byte | Ascii | Short | Long | Rational | Sbyte | Undefined
             | Sshort | Slong | Srational | Float | Double

  let field_of_int = function
    | 1 -> Byte
    | 2 -> Ascii
    | 3 -> Short
    | 4 -> Long
    | 5 -> Rational
    | 6 -> Sbyte
    | 7 -> Undefined
    | 8 -> Sshort
    | 9 -> Slong
    | 10 -> Srational
    | 11 -> Float
    | 12 -> Double
    | n -> invalid_arg ("Field kind " ^ string_of_int n)

  let pp_field ppf = function
    | Byte -> Fmt.pf ppf "byte"
    | Ascii -> Fmt.pf ppf "ascii"
    | Short -> Fmt.pf ppf "short"
    | Long -> Fmt.pf ppf "long"
    | Rational -> Fmt.pf ppf "rational"
    | Sbyte -> Fmt.pf ppf "sbyte"
    | Undefined -> Fmt.pf ppf "undefined"
    | Sshort -> Fmt.pf ppf "sshort"
    | Slong -> Fmt.pf ppf "slong"
    | Srational -> Fmt.pf ppf "srational"
    | Float -> Fmt.pf ppf "float"
    | Double -> Fmt.pf ppf "double"

  type tag =
    | ImageWidth
    | ImageLength
    | BitsPerSample
    | Compression
    | PhotometricInterpretation
    | StripOffsets
    | RowsPerStrip
    | StripByteCounts
    | XResolution
    | YResolution
    | PlanarConfiguration
    | ResolutionUnit
    | SampleFormat
    | SamplesPerPixel
    | Unknown of int

  let tag_of_int = function
    | 256 -> ImageWidth
    | 257 -> ImageLength
    | 258 -> BitsPerSample
    | 259 -> Compression
    | 262 -> PhotometricInterpretation
    | 273 -> StripOffsets
    | 277 -> SamplesPerPixel
    | 278 -> RowsPerStrip
    | 279 -> StripByteCounts
    | 282 -> XResolution
    | 283 -> YResolution
    | 284 -> PlanarConfiguration
    | 296 -> ResolutionUnit
    | 339 -> SampleFormat
    | i -> Unknown i

  let pp_tag ppf (x : tag) = match x with
    | ImageWidth -> Fmt.string ppf "image-width"
    | ImageLength -> Fmt.string ppf "image-length"
    | BitsPerSample -> Fmt.string ppf "bits-per-sample"
    | Compression ->Fmt.string ppf  "compression"
    | PhotometricInterpretation -> Fmt.string ppf "photometric-interpretation"
    | StripOffsets -> Fmt.string ppf "strip-offsets"
    | RowsPerStrip -> Fmt.string ppf "rows-per-strip"
    | StripByteCounts -> Fmt.string ppf "strip-byte-counts"
    | XResolution -> Fmt.string ppf "x-resolution"
    | YResolution -> Fmt.string ppf "y-resolution"
    | ResolutionUnit -> Fmt.string ppf "resolution-unit"
    | PlanarConfiguration -> Fmt.string ppf "planar-configuration"
    | SampleFormat -> Fmt.string ppf "sample-format"
    | SamplesPerPixel -> Fmt.string ppf "samples-per-pixel"
    | Unknown i -> Fmt.pf ppf "unknown-%i" i


  type t = {
    entries : entry list;
    data_offsets : int list;
    data_bytecounts : int list;
  } and entry = {
    tag : tag;
    field : field;
    count : int32;
    offset : int32; (* Could also be a value if 4 bytes or less. *)
  }

  let pp_entry ppf e =
    Fmt.pf ppf "tag: %a, field: %a, count: %ld, value/offset: %ld" pp_tag e.tag pp_field e.field e.count e.offset

  let lookup e tag =
    List.find_opt (fun e -> e.tag = tag) e

  let lookup_exn e tag =
    List.find (fun e -> e.tag = tag) e

  let height e = lookup_exn e ImageLength
  let width e = lookup_exn e ImageWidth

  let add_int optint i = Optint.Int63.(add optint (of_int i))

  let get_dataset_offsets endian entries reader =
    let strip_offsets = lookup_exn entries StripOffsets in
    let strips = ref [] in
    let strip_count = Int32.to_int strip_offsets.count in
    let strip_bytes = if strip_offsets.field = Short then 2 else 4 in
    let length = strip_count * strip_bytes in
    let buf = Cstruct.create length in
    let strip_offset = Optint.Int63.of_int32 strip_offsets.offset in
    File.pread_exact ~file_offset:strip_offset reader [ buf ];
    let get_offset ~offset buf = function
      | Short -> Endian.uint16 ~offset endian buf
      | _ -> Endian.uint32 ~offset endian buf |> Int32.to_int
    in
    for i = 0 to strip_count - 1 do
      strips := get_offset ~offset:(i * strip_bytes) buf strip_offsets.field :: !strips
    done;
    List.rev !strips

  let get_bytecounts endian entries reader =
    let strip_offsets = lookup_exn entries StripByteCounts in
    let strips = ref [] in
    let strip_count = Int32.to_int strip_offsets.count in
    let strip_bytes = if strip_offsets.field = Short then 2 else 4 in
    let length = strip_count * strip_bytes in
    let buf = Cstruct.create length in
    let strip_offset = Optint.Int63.of_int32 strip_offsets.offset in
    File.pread_exact ~file_offset:strip_offset reader [ buf ];
    let get_offset ~offset buf = function
      | Short -> Endian.uint16 ~offset endian buf
      | _ -> Endian.uint32 ~offset endian buf |> Int32.to_int
    in
    for i = 0 to strip_count - 1 do
      strips := get_offset ~offset:(i * strip_bytes) buf strip_offsets.field :: !strips
    done;
    List.rev !strips

  let v ~file_offset header reader =
    let endian = header.byte_order in
    let incr_offset = add_int file_offset in
    let size_buf = Cstruct.create 2 in
    let bufs = [ size_buf ] in
    File.pread_exact reader ~file_offset bufs;
    let size = Endian.uint16 endian size_buf in
    let buf = Cstruct.create (12 * size) in
    File.pread_exact reader ~file_offset:(incr_offset 2) [ buf ];
    let entries = ref [] in
    for i = 0 to size - 1 do
      let tag = Endian.uint16 ~offset:(i * 12) endian buf |> tag_of_int in
      let field = Endian.uint16 ~offset:(i * 12 + 2) endian buf |> field_of_int in
      let count = Endian.uint32 ~offset:(i * 12 + 4) endian buf in
      let offset = Endian.uint32 ~offset:(i * 12 + 8) endian buf in
      entries := { tag; field; count; offset } :: !entries
    done;
    let entries = List.rev !entries in
    let data_offsets = get_dataset_offsets endian entries reader in
    let data_bytecounts = get_bytecounts endian entries reader in
    { entries; data_offsets; data_bytecounts }
end

type t = {
  header : header;
  ifd : Ifd.t;
  reader : File.ro;
}

let of_file f =
  let header = header f in
  let ifd =
    Ifd.v ~file_offset:header.offset header f
  in
  { header; ifd; reader = f }
