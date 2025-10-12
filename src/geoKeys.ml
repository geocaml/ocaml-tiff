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

(* type t =  {
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


type ifd = t *)

type key =
  | GTModelTypeGeoKey
  | GTRasterTypeGeoKey
  | GeographicTypeGeoKey
  | GeogCitationGeoKey
  | GeogAngularUnitsGeoKey
  | GeogSemiMajorAxisGeoKey
  | GeogInvFlatteningGeoKey
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

let key_of_id = function
  | 1024 -> GTModelTypeGeoKey
  | 1025 -> GTRasterTypeGeoKey
  | 2048 -> GeographicTypeGeoKey
  | 2049 -> GeogCitationGeoKey
  | 2054 -> GeogAngularUnitsGeoKey
  | 2057 -> GeogSemiMajorAxisGeoKey
  | 2059 -> GeogInvFlatteningGeoKey
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
