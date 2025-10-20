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

type ifd = t

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

type predictor = No_predictor | HorizontalDifferencing | Unknown of int

let predictor_of_int = function
  | 1 -> No_predictor
  | 2 -> HorizontalDifferencing
  | i -> Unknown i

let predictor_to_int = function
  | No_predictor -> 1
  | HorizontalDifferencing -> 2
  | Unknown i -> i

type sample_format =
  | UnsignedInteger
  | SignedInteger
  | IEEEFloatingPoint
  | Undefined
  | Unknown of int

let sample_format_of_int = function
  | 1 -> UnsignedInteger
  | 2 -> SignedInteger
  | 3 -> IEEEFloatingPoint
  | 4 -> Undefined
  | i -> Unknown i

let sample_format_to_int = function
  | UnsignedInteger -> 1
  | SignedInteger -> 2
  | IEEEFloatingPoint -> 3
  | Undefined -> 4
  | Unknown i -> i

let lookup_exn e tag = List.find (fun e -> e.tag = tag) e
