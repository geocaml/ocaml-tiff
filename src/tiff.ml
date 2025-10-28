(* Parses TIFF headers, IFDs (Image File Directories), interprets TIFF tags, manages reading image metadata and pixel data, and handles dispatching decompression logic depending on tag values *)

module File = struct
  type ro = file_offset:Optint.Int63.t -> Cstruct.t list -> unit
  (** Read-only access to a file that supports reading at a particular offset.
      The read should be exact and raise [End_of_file] should that be the case.
  *)
end

type header = {
  kind : tiff_kind;
  byte_order : endianness;
  offset : Optint.Int63.t;
}

and tiff_kind = Tiff | Bigtiff
and endianness = Big | Little

module Endian = struct
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
end

let header ro =
  (* We may get more bytes than we want, but this is to support Bigtiffs *)
  let buf = Cstruct.create 16 in
  ro ~file_offset:Optint.Int63.zero [ buf ];
  let byte_order =
    match Cstruct.to_string ~off:0 ~len:2 buf with
    | "II" -> Little
    | "MM" -> Big
    | e -> failwith ("Unknown endianness of TIFF file " ^ e)
  in
  let magic = Endian.uint16 ~offset:2 byte_order buf in
  let kind, offset =
    match magic with
    | 42 ->
        (Tiff, Endian.uint32 ~offset:4 byte_order buf |> Optint.Int63.of_int32)
    | 43 ->
        ( Bigtiff,
          Endian.uint64 ~offset:8 byte_order buf |> Optint.Int63.of_int64 )
    | i -> failwith ("Unknown magic number: " ^ string_of_int i)
  in
  { byte_order; kind; offset }

module Ifd = struct
  type field =
    | Byte
    | Ascii
    | Short
    | Long
    | Rational
    | Sbyte
    | Undefined
    | Sshort
    | Slong
    | Srational
    | Float
    | Double
    | Long8
    | Slong8
    | Ifd8

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
    | 16 -> Long8
    | 17 -> Slong8
    | 18 -> Ifd8
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
    | Long8 -> Fmt.pf ppf "long8"
    | Slong8 -> Fmt.pf ppf "slong8"
    | Ifd8 -> Fmt.pf ppf "ifd8"

  let rec field_byte_size (x : field) =
    match x with
    | Byte -> 1
    | Ascii -> 1
    | Short -> 2
    | Long -> 4
    | Rational -> field_byte_size Long * 2
    | Sbyte -> 1
    | Undefined -> 1
    | Sshort -> 2
    | Slong -> 4
    | Srational -> field_byte_size Slong * 2
    | Float -> 4
    | Double -> 8
    | Long8 -> 8
    | Slong8 -> 8
    | Ifd8 -> 8

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
    | ModelPixelScale
    | ModelTiepoint
    | ModelTransformation
    | GeoDoubleParams
    | GeoAsciiParams
    | GeoKeyDirectory
    | Predictor
    | TileWidth
    | TileHeight
    | TileOffsets
    | TileByteCounts
    | GdalMetadata
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
    | 33550 -> ModelPixelScale
    | 33922 -> ModelTiepoint
    | 34264 -> ModelTransformation
    | 34736 -> GeoDoubleParams
    | 34737 -> GeoAsciiParams
    | 34735 -> GeoKeyDirectory
    | 317 -> Predictor
    | 322 -> TileWidth
    | 323 -> TileHeight
    | 324 -> TileOffsets
    | 325 -> TileByteCounts
    | 42112 -> GdalMetadata
    | i -> Unknown i

  let pp_tag ppf (x : tag) =
    match x with
    | ImageWidth -> Fmt.string ppf "image-width"
    | ImageLength -> Fmt.string ppf "image-length"
    | BitsPerSample -> Fmt.string ppf "bits-per-sample"
    | Compression -> Fmt.string ppf "compression"
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
    | ModelPixelScale -> Fmt.string ppf "model-pixel-scale"
    | ModelTiepoint -> Fmt.string ppf "model-tiepoint"
    | ModelTransformation -> Fmt.string ppf "model-transformation"
    | GeoDoubleParams -> Fmt.string ppf "geo-double-params"
    | GeoAsciiParams -> Fmt.string ppf "geo-ascii-params"
    | GeoKeyDirectory -> Fmt.string ppf "geo-key-directory"
    | Predictor -> Fmt.string ppf "predictor"
    | TileWidth -> Fmt.string ppf "tile-width"
    | TileHeight -> Fmt.string ppf "tile-height"
    | TileOffsets -> Fmt.string ppf "tile-offsets"
    | TileByteCounts -> Fmt.string ppf "tile-byte-counts"
    | GdalMetadata -> Fmt.string ppf "gdal-metadata"
    | Unknown i -> Fmt.pf ppf "unknown-%i" i

  type t = {
    entries : entry list;
    data_offsets : int list;
    data_bytecounts : int list;
    header : header;
    ro : File.ro;
  }

  and entry = {
    tag : tag;
    field : field;
    count : int64;
    offset : int64; (* Overallocate for normal tiffs but needed for bigtiffs *)
  }

  type compression =
    | No_compression
    | CCITTRLE
    | PACKBITS
    | CCITTFAX3
    | CCITTFAX4
    | LZW
    | OJPEG
    | JPEG
    | DEFLATE
    | ADOBE_DEFLATE
    | Other of int

  let compression_to_string = function
    | No_compression -> "No_compression"
    | CCITTRLE -> "CCITTRLE"
    | PACKBITS -> "PACKBITS"
    | CCITTFAX3 -> "CCITTFAX3"
    | CCITTFAX4 -> "CCITTFAX4"
    | LZW -> "LZW"
    | OJPEG -> "OJPEG"
    | JPEG -> "JPEG"
    | DEFLATE -> "DEFLATE"
    | ADOBE_DEFLATE -> "ADOBE_DEFLATE"
    | Other i -> "Other " ^ string_of_int i

  let compression_of_int = function
    | 1 -> No_compression
    | 2 -> CCITTRLE
    | 32773 -> PACKBITS
    | 3 -> CCITTFAX3
    | 4 -> CCITTFAX4
    | 5 -> LZW
    | 6 -> OJPEG
    | 7 -> JPEG
    | 32946 -> DEFLATE
    | 8 -> ADOBE_DEFLATE
    | i -> Other i

  let compression_to_int = function
    | No_compression -> 1
    | CCITTRLE -> 2
    | PACKBITS -> 32773
    | CCITTFAX3 -> 3
    | CCITTFAX4 -> 4
    | LZW -> 5
    | OJPEG -> 6
    | JPEG -> 7
    | DEFLATE -> 32946
    | ADOBE_DEFLATE -> 8
    | Other i -> i

type header = Ifd.header

type ('repr, 'kind) kind =
  | Uint8 : (int, Bigarray.int8_unsigned_elt) kind
  | Int8 : (int, Bigarray.int8_signed_elt) kind
  | Uint16 : (int, Bigarray.int16_unsigned_elt) kind
  | Int16 : (int, Bigarray.int16_signed_elt) kind
  | Int32 : (int32, Bigarray.int32_elt) kind
  | Float32 : (float, Bigarray.float32_elt) kind
  | Float64 : (float, Bigarray.float64_elt) kind

type ('repr, 'kind) t = {
  data_type : ('repr, 'kind) kind;
  header : header;
  ifd : Ifd.t;
}

let ifd t = t.ifd

let from_file (type a b) (data_type : (a, b) kind) (f : File.ro) : (a, b) t =
  let header = Ifd.header f in
  let ifd = Ifd.v ~file_offset:header.offset header f in
  { data_type; header; ifd }

type window = { xoff : int; yoff : int; xsize : int; ysize : int }

module Data = struct
  open Bigarray

  type ('repr, 'kind) t = ('repr, 'kind, c_layout) Genarray.t

  let read_uint8_value buf buf_index _ = Cstruct.get_uint8 buf buf_index

  let read_int8_value buf buf_index _ =
    (Cstruct.get_uint8 buf buf_index lsl (Sys.int_size - 8))
    asr (Sys.int_size - 8)
  (* as per Bytes.get_int8 *)

  let read_uint16_value buf buf_index tiff_endianness =
    Endian.uint16 ~offset:buf_index tiff_endianness buf

  let read_int16_value buf buf_index tiff_endianness =
    Endian.int16 ~offset:buf_index tiff_endianness buf

  let read_int32_value buf buf_index tiff_endianness =
    Endian.uint32 ~offset:buf_index tiff_endianness buf

  let read_float32_value buf buf_index tiff_endianness =
    let int_value = Endian.uint32 ~offset:buf_index tiff_endianness buf in
    Int32.float_of_bits int_value

  let read_float64_value buf buf_index tiff_endianness =
    let int_value = Endian.uint64 ~offset:buf_index tiff_endianness buf in
    Int64.float_of_bits int_value

  let ceil a b = (a + b - 1) / b

  let read_data t ro plane window arr_type read_value =
    let ifd = t.ifd in
    let samples_per_pixel = Ifd.samples_per_pixel ifd in
    let height = Ifd.height ifd in
    let width = Ifd.width ifd in
    let bits_per_sample = Ifd.bits_per_sample ifd in
    let planar_configuration = Ifd.planar_configuration ifd in
    let bytes_per_pixel =
      match planar_configuration with
      | Chunky -> List.fold_left Int.add 0 bits_per_sample / 8
      | Planar -> List.nth bits_per_sample plane / 8
      | Unknown _ ->
          failwith "Should not get this far if planar config isn't recognised."
    in
    let strip_offsets = Array.of_list (Ifd.data_offsets ifd) in
    let strip_bytecounts = Array.of_list (Ifd.data_bytecounts ifd) in
    let rows_per_strip = Ifd.rows_per_strip ifd in
    let tiff_endianness = t.header.byte_order in
    let strip_offsets_length = Array.length strip_offsets in
    if strip_offsets_length = Array.length strip_bytecounts then (
      let arr =
        if samples_per_pixel = 1 || planar_configuration = Planar then
          Genarray.create arr_type c_layout [| window.ysize; window.xsize |]
        else
          Genarray.create arr_type c_layout
            [| window.ysize; window.xsize; samples_per_pixel |]
      in

      let strips_per_plane =
        match planar_configuration with
        | Planar -> Array.length strip_offsets / List.length bits_per_sample
        | Chunky -> Array.length strip_offsets
        | Unknown _ ->
            failwith "should not get this far if planar config isn't recognised"
      in

      (* The strip where our first row will be found *)
      let first_strip =
        (window.yoff / rows_per_strip) + (strips_per_plane * plane)
      in
      (* The strip where our last row will be found *)
      let last_strip = first_strip + ceil window.ysize rows_per_strip in
      let row_index =
        ref ((first_strip - (strips_per_plane * plane)) * rows_per_strip)
      in

      (* We iterate through every strip *)
      for strip = first_strip to last_strip - 1 do
        let raw_strip_length = strip_bytecounts.(strip)
        and raw_strip_offset = strip_offsets.(strip) in
        let sparse = raw_strip_length = 0 && raw_strip_offset = 0 in

        (* Calculate the number of rows in the current strip *)
        let rows_in_strip =
          (* If we are the last strip in a plane, then we might have fewer rows *)
          if strip = strips_per_plane - 1 then height - (strip * rows_per_strip)
          else rows_per_strip
        in

        let expected_size = rows_in_strip * width * bytes_per_pixel in

        let strip_length =
          match sparse with false -> raw_strip_length | true -> expected_size
        in

        let raw_strip_buffer = Cstruct.create strip_length in
        let strip_offset = Optint.Int63.of_int raw_strip_offset in

        (* Fill the strip buffer *)
        if not sparse then ro ~file_offset:strip_offset [ raw_strip_buffer ];

        let strip_buffer =
          match (sparse, Ifd.compression ifd) with
          | true, _ | false, No_compression ->
              if Cstruct.length raw_strip_buffer < expected_size then
                failwith "Strip is unexpectedly short";
              raw_strip_buffer
          | false, LZW ->
              let uncompressed_buffer = Cstruct.create expected_size in
              Lzw.decode raw_strip_buffer uncompressed_buffer;
              uncompressed_buffer
          | _ -> failwith "Unsupported compression"
        in

        (* Iterating through the rows of the current strip *)
        for inner_row = 0 to rows_in_strip - 1 do
          (* The actual y offset we are at *)
          let y_offset = !row_index + inner_row in

          if y_offset >= window.yoff + window.ysize || y_offset < window.yoff
          then ()
          else
            (* The x offset we are at *)
            let x_offset = ref window.xoff in

            while !x_offset <= window.xoff + window.xsize - 1 do
              (* The index into the strip buffer *)
              let index = !x_offset + (inner_row * width) in
              if samples_per_pixel = 1 || planar_configuration = Planar then
                let value =
                  read_value strip_buffer (index * bytes_per_pixel)
                    tiff_endianness
                in
                Genarray.set arr [| y_offset - window.yoff; !x_offset |] value
              else
                for channel = 0 to samples_per_pixel - 1 do
                  let byte_off =
                    if channel > 0 then List.nth bits_per_sample channel / 8
                    else 0
                  in
                  let value =
                    read_value strip_buffer
                      ((index * bytes_per_pixel) + byte_off)
                      tiff_endianness
                  in
                  Genarray.set arr [| y_offset; !x_offset; channel |] value
                done;
              x_offset := !x_offset + 1
            done
        done;
        row_index := !row_index + rows_in_strip
      done;
      arr)
    else
      raise
        (Invalid_argument
           "strip_offsets and strip_bytecounts are of different lengths")
end

(* have to specify all 4 or else it defaults to whole file*)
let data (type repr kind) ?plane ?window (t : (repr, kind) t) (f : File.ro) :
    (repr, kind) Data.t =
  let ifd = ifd t in
  let planar_configuration = Ifd.planar_configuration ifd in
  let plane =
    match (planar_configuration, plane) with
    | Planar, Some p -> p
    | Planar, None -> invalid_arg "Must specify plane for data read"
    | Chunky, None | Chunky, Some 0 -> 0
    | Chunky, Some _ -> invalid_arg "Can not select plane on single plane TIFFs"
    | Unknown _, _ -> invalid_arg "Unknown planar format TIFF"
  in
  let window =
    match window with
    | Some w -> w
    | None ->
        let width = Ifd.width ifd in
        let height = Ifd.height ifd in
        { xoff = 0; yoff = 0; xsize = width; ysize = height }
  in
  let sample_format = Ifd.sample_format ifd
  and bpp = List.nth (Ifd.bits_per_sample ifd) plane in
  match (t.data_type, sample_format, bpp) with
  | Uint8, UnsignedInteger, 8 ->
      Data.read_data t f plane window Bigarray.int8_unsigned
        Data.read_uint8_value
  | Int8, SignedInteger, 8 ->
      Data.read_data t f plane window Bigarray.int8_signed Data.read_int8_value
  | Uint16, UnsignedInteger, 16 ->
      Data.read_data t f plane window Bigarray.int16_unsigned
        Data.read_uint16_value
  | Int16, SignedInteger, 16 ->
      Data.read_data t f plane window Bigarray.int16_signed
        Data.read_int16_value
  | Int32, SignedInteger, 32 ->
      Data.read_data t f plane window Bigarray.int32 Data.read_int32_value
  | Float32, IEEEFloatingPoint, 32 ->
      Data.read_data t f plane window Bigarray.float32 Data.read_float32_value
  | Float64, IEEEFloatingPoint, 64 ->
      Data.read_data t f plane window Bigarray.float64 Data.read_float64_value
  | _ -> raise (Invalid_argument "datatype not correct for plane")

module Private = struct
  module Lzw = Lzw
end
