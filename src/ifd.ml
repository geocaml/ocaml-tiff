(* Parses and interprets TIFF Image File Directory (IFD) metadata *)

type header = {
  kind : tiff_kind;
  byte_order : endianness;
  offset : Optint.Int63.t;
}

and tiff_kind = Tiff | Bigtiff
and endianness = Endian.endianness

let read_header ro =
  (* We may get more bytes than we want, but this is to support Bigtiffs *)
  let buf = Cstruct.create 16 in
  ro ~file_offset:Optint.Int63.zero [ buf ];
  let byte_order =
    match Cstruct.to_string ~off:0 ~len:2 buf with
    | "II" -> Endian.Little
    | "MM" -> Endian.Big
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

let write_header wo header =
  let buf = Cstruct.create 16 in

  Cstruct.blit_from_string
    (match header.byte_order with Endian.Little -> "II" | Endian.Big -> "MM")
    0 buf 0 2;

  (match header.kind with
  | Tiff ->
      Endian.set_uint16 ~offset:2 header.byte_order buf 42;
      Endian.set_uint32 ~offset:4 header.byte_order buf
        (header.offset |> Optint.Int63.to_int32)
  | Bigtiff ->
      Endian.set_uint16 ~offset:2 header.byte_order buf 43;
      Endian.set_uint64 ~offset:8 header.byte_order buf
        (header.offset |> Optint.Int63.to_int64));

  wo ~file_offset:Optint.Int63.zero [ buf ]

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

let field_to_int = function
  | Byte -> 1
  | Ascii -> 2
  | Short -> 3
  | Long -> 4
  | Rational -> 5
  | Sbyte -> 6
  | Undefined -> 7
  | Sshort -> 8
  | Slong -> 9
  | Srational -> 10
  | Float -> 11
  | Double -> 12
  | Long8 -> 16
  | Slong8 -> 17
  | Ifd8 -> 18

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

let tag_to_int = function
  | ImageWidth -> 256
  | ImageLength -> 257
  | BitsPerSample -> 258
  | Compression -> 259
  | PhotometricInterpretation -> 262
  | StripOffsets -> 273
  | SamplesPerPixel -> 277
  | RowsPerStrip -> 278
  | StripByteCounts -> 279
  | XResolution -> 282
  | YResolution -> 283
  | PlanarConfiguration -> 284
  | ResolutionUnit -> 296
  | SampleFormat -> 339
  | ModelPixelScale -> 33550
  | ModelTiepoint -> 33922
  | ModelTransformation -> 34264
  | GeoDoubleParams -> 34736
  | GeoAsciiParams -> 34737
  | GeoKeyDirectory -> 34735
  | Predictor -> 317
  | TileWidth -> 322
  | TileHeight -> 323
  | TileOffsets -> 324
  | TileByteCounts -> 325
  | GdalMetadata -> 42112
  | Unknown i -> i

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
  next_ifd : Optint.Int63.t;
}

and entry = {
  tag : tag;
  field : field;
  count : int64;
  offset : int64; (* Overallocate for normal tiffs but needed for bigtiffs *)
  is_immediate : bool;
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

let pp_sample_format ppf = function
  | UnsignedInteger -> Fmt.string ppf "Unsigned Integer"
  | SignedInteger -> Fmt.string ppf "Signed Integer"
  | IEEEFloatingPoint -> Fmt.string ppf "IEEEFloatingPoint"
  | Undefined -> Fmt.string ppf "Undefined"
  | Unknown n -> Fmt.pf ppf "Unknown %i" n

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

type planar_configuration = Chunky | Planar | Unknown of int

let planar_configuration_of_int = function
  | 1 -> Chunky
  | 2 -> Planar
  | i -> Unknown i

let planar_configuration_to_int = function
  | Chunky -> 1
  | Planar -> 2
  | Unknown i -> i

let entries t = t.entries
let data_offsets t = t.data_offsets
let data_bytecounts t = t.data_bytecounts

let pp_entry ppf e =
  Fmt.pf ppf "tag: %a, field: %a, count: %Ld, value/offset: %Ld" pp_tag e.tag
    pp_field e.field e.count e.offset

let lookup e tag = List.find_opt (fun e -> e.tag = tag) e
let lookup_exn e tag = List.find (fun e -> e.tag = tag) e

let read_entry e =
  match e.field with
  | Short -> e.offset |> Int64.to_int
  | Long -> e.offset |> Int64.to_int
  | Long8 -> e.offset |> Int64.to_int
  | _ ->
      raise
        (Invalid_argument (Fmt.str "Bad entry for short read: %a" pp_entry e))

let height e = lookup_exn e.entries ImageLength |> read_entry
let width e = lookup_exn e.entries ImageWidth |> read_entry
let rows_per_strip e = lookup_exn e.entries RowsPerStrip |> read_entry

let samples_per_pixel e =
  try lookup_exn e.entries SamplesPerPixel |> read_entry with Not_found -> 1

let add_int optint i = Optint.Int63.(add optint (of_int i))

let is_immediate_raw ~count field =
  let total_size = Int64.mul count (Int64.of_int (field_byte_size field)) in
  Int64.compare total_size 4L <= 0

let is_immediate_raw_big ~count field =
  let total_size = Int64.mul count (Int64.of_int (field_byte_size field)) in
  Int64.compare total_size 8L <= 0

let get_dataset_offsets endian entries reader =
  match lookup entries StripOffsets with
  | None -> []
  | Some strip_offsets ->
      let strip_count = Int64.to_int strip_offsets.count in
      if strip_offsets.is_immediate then [ read_entry strip_offsets ]
      else
        let strips = ref [] in
        let strip_bytes =
          match strip_offsets.field with
          | Short -> 2
          | Long -> 4
          | Long8 -> 8
          | _ ->
              Fmt.failwith "Unsupported strip length: %a" pp_field
                strip_offsets.field
        in
        let length = strip_count * strip_bytes in
        let buf = Cstruct.create length in
        let strip_offset = Optint.Int63.of_int64 strip_offsets.offset in
        reader ~file_offset:strip_offset [ buf ];
        let get_offset ~offset buf = function
          | Short -> Endian.uint16 ~offset endian buf
          | Long -> Endian.uint32 ~offset endian buf |> Int32.to_int
          | Long8 -> Endian.uint64 ~offset endian buf |> Int64.to_int
          | _ -> Fmt.failwith "Unsupported"
        in
        for i = 0 to strip_count - 1 do
          strips :=
            get_offset ~offset:(i * strip_bytes) buf strip_offsets.field
            :: !strips
        done;
        List.rev !strips

let get_bytecounts endian entries reader =
  match lookup entries StripByteCounts with
  | None -> []
  | Some strip_offsets ->
      if strip_offsets.is_immediate then [ read_entry strip_offsets ]
      else
        let strip_count = Int64.to_int strip_offsets.count in
        let strips = ref [] in
        let strip_bytes =
          match strip_offsets.field with
          | Short -> 2
          | Long -> 4
          | Long8 -> 8
          | _ ->
              Fmt.failwith "Unsupported strip length: %a" pp_field
                strip_offsets.field
        in
        let length = strip_count * strip_bytes in
        let buf = Cstruct.create length in
        let strip_offset = Optint.Int63.of_int64 strip_offsets.offset in
        reader ~file_offset:strip_offset [ buf ];
        let get_offset ~offset buf = function
          | Short -> Endian.uint16 ~offset endian buf
          | Long -> Endian.uint32 ~offset endian buf |> Int32.to_int
          | Long8 -> Endian.uint64 ~offset endian buf |> Int64.to_int
          | _ ->
              Fmt.failwith "Unsupported strip length: %a" pp_field
                strip_offsets.field
        in
        for i = 0 to strip_count - 1 do
          strips :=
            get_offset ~offset:(i * strip_bytes) buf strip_offsets.field
            :: !strips
        done;
        List.rev !strips

let max_group lst n =
  let rec loop acc t =
    match (acc, t) with
    | [], _ -> assert false
    | _, [] -> List.rev acc
    | a :: acc, v :: vs ->
        if List.length a >= n then loop ([ v ] :: List.rev a :: acc) vs
        else loop ((v :: a) :: acc) vs
  in
  loop [ [] ] lst

let read_entry_raw' offset field count reader =
  let file_offset = offset |> Optint.Int63.of_int64 in
  let byte_size = field_byte_size field in
  let bufs = List.init count (fun _ -> Cstruct.create byte_size) in
  let groups = if count > 256 then max_group bufs 256 else [ bufs ] in
  let run () =
    (* TODO: Common Fiber.List.map abstraction ? *)
    List.fold_left
      (fun file_offset bufs ->
        reader ~file_offset bufs;
        Optint.Int63.add file_offset
          (Optint.Int63.of_int
             (List.fold_left (fun acc buf -> Cstruct.length buf + acc) 0 bufs)))
      file_offset groups
  in
  let (_ : Optint.Int63.t) = run () in
  List.concat groups

let read_entry_raw ?count entry =
  let count =
    match count with Some c -> c | None -> entry.count |> Int64.to_int
  in
  read_entry_raw' entry.offset entry.field count

let pixel_scale t =
  let entry = lookup_exn t.entries ModelPixelScale in
  let scales = read_entry_raw entry t.ro in
  assert (List.length scales = 3);
  List.map (Endian.double t.header.byte_order) scales |> Array.of_list

let bits_per_sample t =
  let entry = lookup_exn t.entries BitsPerSample in
  if entry.count = 1L then [ Int64.to_int entry.offset ]
  else if entry.count = 2L then (
    let buf = Cstruct.create 8 in
    Cstruct.BE.set_uint64 buf 0 entry.offset;
    [ Cstruct.BE.get_uint16 buf 0; Cstruct.BE.get_uint16 buf 2 ])
  else if entry.count = 3L && t.header.kind = Bigtiff then (
    let buf = Cstruct.create 8 in
    Cstruct.BE.set_uint64 buf 0 entry.offset;
    [
      Cstruct.BE.get_uint16 buf 2;
      Cstruct.BE.get_uint16 buf 2;
      Cstruct.BE.get_uint16 buf 4;
    ])
  else if entry.count = 4L && t.header.kind = Bigtiff then (
    let buf = Cstruct.create 8 in
    Cstruct.BE.set_uint64 buf 0 entry.offset;
    [
      Cstruct.BE.get_uint16 buf 2;
      Cstruct.BE.get_uint16 buf 2;
      Cstruct.BE.get_uint16 buf 4;
      Cstruct.BE.get_uint16 buf 6;
    ])
  else
    let scales = read_entry_raw entry t.ro in
    List.map (Endian.uint16 t.header.byte_order) scales

let planar_configuration t =
  try
    lookup_exn t.entries PlanarConfiguration
    |> read_entry |> planar_configuration_of_int
  with Not_found -> Chunky

let compression t =
  lookup_exn t.entries Compression |> read_entry |> compression_of_int

let tiepoint t =
  let entry = lookup_exn t.entries ModelTiepoint in
  let scales = read_entry_raw entry t.ro in
  assert (List.length scales mod 6 = 0);
  List.map (Endian.double t.header.byte_order) scales |> Array.of_list

let transformation t =
  let entry = lookup_exn t.entries ModelTransformation in
  let scales = read_entry_raw entry t.ro in
  assert (List.length scales = 16);
  List.map (Endian.double t.header.byte_order) scales |> Array.of_list

let geo_double_params t =
  let entry = lookup_exn t.entries GeoDoubleParams in
  let doubles = read_entry_raw entry t.ro in
  Array.map (Endian.double t.header.byte_order) (Array.of_list doubles)

let string_of_int64 endianness i =
  let buf = Cstruct.create 8 in
  if endianness = Endian.Big then Cstruct.BE.set_uint64 buf 0 i;
  if endianness = Endian.Little then Cstruct.LE.set_uint64 buf 0 i;
  Cstruct.to_string buf

let geo_ascii_params t =
  (* https://docs.ogc.org/is/19-008r4/19-008r4.html#_requirements_class_geoasciiparamstag *)
  let entry = lookup_exn t.entries GeoAsciiParams in
  let s =
    if entry.count <= 8L then string_of_int64 t.header.byte_order entry.offset
    else
      let ascii = read_entry_raw entry t.ro in
      Cstruct.concat ascii |> Cstruct.to_string
  in
  let len = String.length s in
  (* Remove the ASCII null byte *)
  let s = String.sub s 0 (len - 1) in
  String.split_on_char '|' s |> List.filter (fun v -> not (String.equal "" v))

module GeoKeys = struct
  type ifd = t

  type key =
    | GTModelTypeGeoKey
    | GTRasterTypeGeoKey
    | GeographicTypeGeoKey
    | GeogCitationGeoKey
    | GeogAngularUnitsGeoKey
    | GeogSemiMajorAxisGeoKey
    | GeogInvFlatteningGeoKey
    | ProjectedCRS
    | Unknown of int

  let pp_key ppf = function
    | Unknown i -> Fmt.pf ppf "unknown-%i" i
    | GTModelTypeGeoKey -> Fmt.string ppf "gt-model-type"
    | GTRasterTypeGeoKey -> Fmt.string ppf "gt-raster-type"
    | GeographicTypeGeoKey -> Fmt.string ppf "geographic-type"
    | GeogCitationGeoKey -> Fmt.string ppf "geog-citation"
    | GeogAngularUnitsGeoKey -> Fmt.string ppf "geog-angular-units"
    | GeogSemiMajorAxisGeoKey -> Fmt.string ppf "geog-semi-major-axis"
    | GeogInvFlatteningGeoKey -> Fmt.string ppf "geog-inv-flattening"
    | ProjectedCRS -> Fmt.string ppf "projected-crs"

  let key_of_id = function
    | 1024 -> GTModelTypeGeoKey
    | 1025 -> GTRasterTypeGeoKey
    | 2048 -> GeographicTypeGeoKey
    | 2049 -> GeogCitationGeoKey
    | 2054 -> GeogAngularUnitsGeoKey
    | 2057 -> GeogSemiMajorAxisGeoKey
    | 2059 -> GeogInvFlatteningGeoKey
    | 3072 -> ProjectedCRS
    | i -> Unknown i

  type model_type = Projected | Geographic | Geocentric | Other of int

  let model_type_of_int = function
    | 1 -> Projected
    | 2 -> Geographic
    | 3 -> Geocentric
    | i -> Other i

  type raster_type = RasterPixelIsArea | RasterPixelIsPoint | Other of int

  let raster_type_of_int = function
    | 1 -> RasterPixelIsArea
    | 2 -> RasterPixelIsPoint
    | i -> Other i

  type angular_units =
    | Radian
    | Degree
    | Arc_minute
    | Arc_second
    | Grad
    | Gon
    | DMS
    | DMS_hemisphere

  let angular_units_of_int = function
    | 9101 -> Radian
    | 9102 -> Degree
    | 9103 -> Arc_minute
    | 9104 -> Arc_second
    | 9105 -> Grad
    | 9106 -> Gon
    | 9107 -> DMS
    | 9108 -> DMS_hemisphere
    | _ -> failwith "Unknown angular units"

  let angular_units_to_string = function
    | Radian -> "radian"
    | Degree -> "degree"
    | Arc_minute -> "arc-minute"
    | Arc_second -> "arc-second"
    | Grad -> "grad"
    | Gon -> "gon"
    | DMS -> "dms"
    | DMS_hemisphere -> "dms-hemisphere"

  type entry = {
    key : key;
    field : [ `Immediate | `Loc of int ];
    count : int;
    offset : int64; (* Overallocate for normal tiffs but needed for bigtiffs *)
  }

  let pp_field ppf = function
    | `Immediate -> Fmt.pf ppf "imm"
    | `Loc i -> pp_tag ppf (tag_of_int i)

  let pp_entry ppf e =
    Fmt.pf ppf "geokey: %a, field: %a, count: %i, value/offset: %Ld" pp_key
      e.key pp_field e.field e.count e.offset

  type t = {
    version : int;
    revision : int;
    minor : int;
    geo_entries : entry list;
  }

  let entries t =
    let entry = lookup_exn t.entries GeoKeyDirectory in
    let ascii = read_entry_raw entry t.ro in
    let values = List.map (Endian.uint16 t.header.byte_order) ascii in
    match values with
    | version :: revision :: minor :: _count :: rest ->
        let rec loop acc = function
          | key :: field :: count :: voff :: more ->
              let k = key_of_id key in
              let f = if field = 0 then `Immediate else `Loc field in
              let e =
                { key = k; field = f; count; offset = Int64.of_int voff }
              in
              loop (e :: acc) more
          | _ -> List.rev acc
        in
        let geo_entries = loop [] rest in
        { version; revision; minor; geo_entries }
    | _ -> invalid_arg "GeoKeyDirectory Malformed!"

  let get_geo_entries t = t.geo_entries

  let pp ppf t =
    Fmt.pf ppf "version: %i, revision: %i, minor: %i, count: %i\n" t.version
      t.revision t.minor
      (List.length t.geo_entries);
    Fmt.(list ~sep:Fmt.cut pp_entry) ppf t.geo_entries

  let lookup_key_exn e key = List.find (fun e -> e.key = key) e

  let model_type e =
    let key = lookup_key_exn e.geo_entries GTModelTypeGeoKey in
    key.offset |> Int64.to_int |> model_type_of_int

  let angular_units e =
    let key = lookup_key_exn e.geo_entries GeogAngularUnitsGeoKey in
    key.offset |> Int64.to_int |> angular_units_of_int

  let raster_type e =
    let key = lookup_key_exn e.geo_entries GTRasterTypeGeoKey in
    key.offset |> Int64.to_int |> raster_type_of_int

  let projected_crs e =
    let entry = lookup_key_exn e.geo_entries ProjectedCRS in
    Int64.to_int entry.offset

  let geo_citation t e =
    let key = lookup_key_exn e.geo_entries GeogCitationGeoKey in
    match key.field with
    | `Immediate -> assert false
    | `Loc i -> (
        match tag_of_int i with
        | GeoAsciiParams ->
            List.nth (geo_ascii_params t) (Int64.to_int key.offset)
        | _ -> "Unknown")

  let semi_major_axis t e =
    let key = lookup_key_exn e.geo_entries GeogSemiMajorAxisGeoKey in
    match key.field with
    | `Immediate -> assert false
    | `Loc i -> (
        match tag_of_int i with
        | GeoDoubleParams -> (geo_double_params t).(Int64.to_int key.offset)
        | _ -> failwith "Unknown location of semi major axis double")

  let inv_flattening t e =
    let key = lookup_key_exn e.geo_entries GeogInvFlatteningGeoKey in
    match key.field with
    | `Immediate -> assert false
    | `Loc i -> (
        match tag_of_int i with
        | GeoDoubleParams -> (geo_double_params t).(Int64.to_int key.offset)
        | _ -> failwith "Unknown location of semi major axis double")
end

let geo_key_directory t = GeoKeys.entries t

let predictor t =
  try
    let entry = lookup_exn t.entries Predictor in
    Int64.to_int entry.offset |> predictor_of_int
  with Not_found -> No_predictor

let sample_format t =
  try
    let entry = lookup_exn t.entries SampleFormat in
    Int64.to_int entry.offset |> sample_format_of_int
  with Not_found -> UnsignedInteger

let tile_width t =
  let entry = lookup_exn t.entries TileWidth in
  Int64.to_int entry.offset

let tile_height t =
  let entry = lookup_exn t.entries TileHeight in
  Int64.to_int entry.offset

let tiles_across t =
  let twidth = tile_width t in
  (width t + twidth - 1) / twidth

let tiles_down t =
  let theight = tile_height t in
  (height t + theight - 1) / theight

let tile_offsets t =
  let entry = lookup_exn t.entries TileOffsets in
  let offsets = read_entry_raw entry t.ro in
  List.map (Endian.uint64 t.header.byte_order) offsets

let tile_byte_counts t =
  let entry = lookup_exn t.entries TileByteCounts in
  let offsets = read_entry_raw entry t.ro in
  if field_byte_size entry.field = 4 then
    List.map (Endian.uint32 t.header.byte_order) offsets
    |> List.map Int64.of_int32
  else List.map (Endian.uint64 t.header.byte_order) offsets

let v ~file_offset header reader =
  let endian = header.byte_order in
  let incr_offset = add_int file_offset in
  (* More than Tiff needs, but how much Bigtiff needs *)
  let size_buf = Cstruct.create 8 in
  let bufs = [ size_buf ] in
  reader ~file_offset bufs;
  let read, count =
    match header.kind with
    | Tiff -> (2, Endian.uint16 endian size_buf)
    | Bigtiff -> (8, Endian.uint64 endian size_buf |> Int64.to_int)
  in
  let entry_size = if header.kind = Tiff then 12 else 20 in
  let buf = Cstruct.create (entry_size * count) in
  let next_ifd_buf =
    Cstruct.create (match header.kind with Tiff -> 4 | Bigtiff -> 8)
  in
  reader ~file_offset:(incr_offset read) [ buf; next_ifd_buf ];
  let entries = ref [] in
  for i = 0 to count - 1 do
    match header.kind with
    | Tiff ->
        let base_offset = i * entry_size in
        let tag = Endian.uint16 ~offset:base_offset endian buf |> tag_of_int in
        let field =
          Endian.uint16 ~offset:(base_offset + 2) endian buf |> field_of_int
        in
        let count =
          Endian.uint32 ~offset:(base_offset + 4) endian buf |> Int64.of_int32
        in
        let is_immediate = is_immediate_raw ~count field in
        let offset =
          match (is_immediate, field) with
          | true, Byte ->
              Cstruct.get_uint8 buf (base_offset + 8) |> Int64.of_int
          | true, Short ->
              Endian.uint16 ~offset:(base_offset + 8) endian buf |> Int64.of_int
          | _ ->
              Endian.uint32 ~offset:(base_offset + 8) endian buf
              |> Int64.of_int32
        in
        let entry = { tag; field; count; offset; is_immediate } in
        entries := entry :: !entries
    | Bigtiff ->
        let base_offset = i * entry_size in
        let tag = Endian.uint16 ~offset:base_offset endian buf |> tag_of_int in
        let field =
          Endian.uint16 ~offset:(base_offset + 2) endian buf |> field_of_int
        in
        let count = Endian.uint64 ~offset:(base_offset + 4) endian buf in
        let is_immediate = is_immediate_raw_big ~count field in
        let offset =
          match (is_immediate, field) with
          | true, Byte ->
              Cstruct.get_uint8 buf (base_offset + 12) |> Int64.of_int
          | true, Short ->
              Endian.uint16 ~offset:(base_offset + 12) endian buf
              |> Int64.of_int
          | true, Long ->
              Endian.uint32 ~offset:(base_offset + 12) endian buf
              |> Int64.of_int32
          | _ -> Endian.uint64 ~offset:(base_offset + 12) endian buf
        in
        entries := { tag; field; count; offset; is_immediate } :: !entries
  done;
  let entries = List.rev !entries in
  let next_ifd =
    match header.kind with
    | Tiff ->
        Endian.uint32 ~offset:0 header.byte_order next_ifd_buf
        |> Optint.Int63.of_int32
    | Bigtiff ->
        Endian.uint64 ~offset:0 header.byte_order next_ifd_buf
        |> Optint.Int63.of_int64
  in
  let data_offsets = get_dataset_offsets endian entries reader in
  let data_bytecounts = get_bytecounts endian entries reader in
  { entries; data_offsets; data_bytecounts; ro = reader; header; next_ifd }

let next_ifd t =
  if Optint.Int63.(equal zero t.next_ifd) then None
  else Some (v ~file_offset:t.next_ifd t.header t.ro)

let write_entry_raw entry endian values writer =
  let field_size = field_byte_size entry.field in
  let file_offset = entry.offset |> Optint.Int63.of_int64 in
  let length = (entry.count |> Int64.to_int) * field_size in
  let data = Cstruct.create length in

  (match entry.field with
  | Ascii ->
      List.map (fun v -> Cstruct.get_char v 0) values
      |> List.iteri (fun i v -> Cstruct.set_char data (i * field_size) v)
  | Byte ->
      List.mapi (fun i v -> Cstruct.get_uint8 v (i * field_size)) values
      |> List.iteri (fun i v -> Cstruct.set_uint8 data (i * field_size) v)
  | Short ->
      List.map (Endian.uint16 endian) values
      |> List.iteri (fun i v ->
          Endian.set_uint16 ~offset:(i * field_size) endian data v)
  | Long ->
      List.map (Endian.uint32 endian) values
      |> List.iteri (fun i v ->
          Endian.set_uint32 ~offset:(i * field_size) endian data v)
  | Double ->
      List.map (Endian.double endian) values
      |> List.iteri (fun i v ->
          Endian.set_double ~offset:(i * field_size) endian data v)
  | _ ->
      List.map (Endian.uint64 endian) values
      |> List.iteri (fun i v ->
          Endian.set_uint64 ~offset:(i * field_size) endian data v));

  writer ~file_offset [ data ]

let write_ifd ~file_offset header writer (ifd : t) =
  let endian = header.byte_order in
  let incr_offset = add_int file_offset in
  let size_buf = Cstruct.create 8 in
  let count = List.length ifd.entries in
  let write, _ =
    match header.kind with
    | Tiff -> (2, Endian.set_uint16 endian size_buf count)
    | Bigtiff -> (8, Endian.set_uint64 endian size_buf (count |> Int64.of_int))
  in
  writer ~file_offset [ size_buf ];

  let entry_size = if header.kind = Tiff then 12 else 20 in
  let buf = Cstruct.create (entry_size * count) in
  List.iteri
    (fun i entry ->
      let base_offset = i * entry_size in
      Endian.set_uint16 ~offset:base_offset endian buf (entry.tag |> tag_to_int);
      Endian.set_uint16 ~offset:(base_offset + 2) endian buf
        (entry.field |> field_to_int);

      match header.kind with
      | Tiff -> (
          Endian.set_uint32 ~offset:(base_offset + 4) endian buf
            (entry.count |> Int64.to_int32);
          match (entry.is_immediate, entry.field) with
          | true, Byte ->
              Cstruct.set_uint8 buf (base_offset + 8)
                (entry.offset |> Int64.to_int)
          | true, Short ->
              Endian.set_uint16 ~offset:(base_offset + 8) endian buf
                (entry.offset |> Int64.to_int)
          | false, _ ->
              Endian.set_uint32 ~offset:(base_offset + 8) endian buf
                (entry.offset |> Int64.to_int32);

              let values = read_entry_raw entry ifd.ro in
              write_entry_raw entry endian values writer
          | _ ->
              Endian.set_uint32 ~offset:(base_offset + 8) endian buf
                (entry.offset |> Int64.to_int32))
      | Bigtiff -> (
          Endian.set_uint64 ~offset:(base_offset + 4) endian buf entry.count;
          match (entry.is_immediate, entry.field) with
          | true, Byte ->
              Cstruct.set_uint8 buf (base_offset + 12)
                (entry.offset |> Int64.to_int)
          | true, Short ->
              Endian.set_uint16 ~offset:(base_offset + 12) endian buf
                (entry.offset |> Int64.to_int)
          | true, Long ->
              Endian.set_uint32 ~offset:(base_offset + 12) endian buf
                (entry.offset |> Int64.to_int32)
          | false, _ ->
              Endian.set_uint64 ~offset:(base_offset + 12) endian buf
                entry.offset;
              let values = read_entry_raw entry ifd.ro in
              write_entry_raw entry endian values writer
          | _ ->
              Endian.set_uint64 ~offset:(base_offset + 12) endian buf
                entry.offset))
    ifd.entries;
  writer ~file_offset:(incr_offset write) [ buf ]
