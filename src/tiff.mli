(** {1 TIFF}

    This library provides functions for accessing TIFF files. A TIFF file is a
    way to provide {i raster} images. *)

open Bigarray

module File : sig
  type ro = file_offset:Optint.Int63.t -> Cstruct.t list -> unit
  (** Read-only access to a file that supports reading at a particular offset.
      The read should be exact and raise [End_of_file] should that be the case.
  *)
end

module Ifd : sig
  type t
  (** An image file directory *)

  type entry
  (** The type of an entry in the image file directory *)

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

  val compression_to_int : compression -> int

  val pp_entry : entry Fmt.t
  (** A pretty printer for IFD entries *)

  val lookup : entry list -> tag -> entry option
  (** Lookup a particular {! tag} in the entries list. *)

  val lookup_exn : entry list -> tag -> entry
  (** Same as {! lookup} but will raise [Not_found]. *)

  val entries : t -> entry list
  (** Access the list of entries in the IFD. *)

  (** {2 IFD Accessor functions} *)

  val width : t -> int
  (** [width t] returns the width of the image. *)

  val height : t -> int
  (** [height t] returns the height of the image. *)

  val rows_per_strip : t -> int
  (** [rows_per_strip t] returns the number of rows per strip. *)

  val samples_per_pixel : t -> int
  (** [samples_per_pixel] is usually 1 for grayscale and 3 for RGB. *)

  val bits_per_sample : t -> int list
  (** [bits_per_sample t] is the number of bits per component corresponding to a
      pixel. The following invariant should hold
      [List.length (bits_per_sample t) = sample_per_pixel t]. *)

  val compression : t -> compression
  (** Compression scheme used in the image data *)

  val tile_width : t -> int
  (** If the tiff is tiled, then the width of individual tiles. *)

  val tiles_across : t -> int
  (** The number of tiles across (width). *)

  val tiles_down : t -> int
  (** The number of tiles down (height). *)

  val tile_height : t -> int
  (** If the tiff is tiled, then the height of individual tiles. *)

  val tile_offsets : t -> Int64.t list
  (** The offsets for each tile in the data. *)

  val tile_byte_counts : t -> Int64.t list
  (** The number of bytes stored in each tile. *)

  val predictor : t -> int
  val planar_configuration : t -> int

  (** {3 GeoTIFF Specific} *)

  val pixel_scale : t -> float array
  (** Pixel scales entry. *)

  val tiepoint : t -> float array
  (** Also known as GeoreferenceTag, this stores raster to model tiepoint pairs.
  *)

  val geo_double_params : t -> float array
  (** Double valued GeoKeys. *)

  val geo_ascii_params : t -> string list
  (** All of the ASCII valued GeoKeys. *)

  module GeoKeys : sig
    type ifd = t
    type t
    type entry
    type model_type = Projected | Geographic | Geocentric | Other of int
    type raster_type = RasterPixelIsArea | RasterPixelIsPoint | Other of int

    type angular_units =
      | Radian
      | Degree
      | Arc_minute
      | Arc_second
      | Grad
      | Gon
      | DMS
      | DMS_hemisphere

    val angular_units_to_string : angular_units -> string
    val entries : ifd -> t
    val get_geo_entries : t -> entry list
    val pp_entry : entry Fmt.t
    val pp : t Fmt.t
    val model_type : t -> model_type
    val raster_type : t -> raster_type
    val angular_units : t -> angular_units
    val geo_citation : ifd -> t -> string
    val semi_major_axis : ifd -> t -> float
    val inv_flattening : ifd -> t -> float
  end

  val geo_key_directory : t -> GeoKeys.t
  (** This tag may be used to store the GeoKey Directory, which defines and
      references the "GeoKeys" *)

  val data_offsets : t -> int list
  (** The offsets into the file for the chunks of the data. *)

  val data_bytecounts : t -> int list
  (** The number of bytes of each chunk of data. *)

  (** {2 Reading entries} *)

  (* val read_entry_short : entry -> int
     (** Reads the value of the entry as a short if the entry field matches
         otherwise it will raise [Invalid_argument _]. *) *)

  val read_entry : entry -> int
  (** Reads the value of the entry as a short if the entry field matches
      otherwise it will raise [Invalid_argument _]. *)

  val read_entry_raw : ?count:int -> entry -> File.ro -> Cstruct.t list
  (** Read entries as raw bytes. This will return a buffer list based on the
      layout of the entry. For example, if the count is [10] and the tag is
      [Double] then you will gexwt back a list of [10] buffers each of length
      [2]. *)
end

type window = { xoff : int; yoff : int; xsize : int; ysize : int }
(** A window can be used to reduce the size of data returned by {! data} *)

module Data : sig
  type ('repr, 'kind) kind =
    | Uint8 : (int, int8_unsigned_elt) kind
    | Float32 : (float, float32_elt) kind  (** A subset of {! Bigarray.kind}. *)

  type ('repr, 'kind) t = ('repr, 'kind, c_layout) Genarray.t
  (** Raw TIFF data. *)
end

type t
(** A TIFF file *)

val from_file : File.ro -> t
(** Start reading a TIFF file. *)

val ifd : t -> Ifd.t
(** Access the IFD of the TIFF file *)

val data :
  ?window:window ->
  t ->
  File.ro ->
  ('repr, 'kind) Data.kind ->
  ('repr, 'kind) Data.t
(** Low-level access to the raw data inside the TIFF file. The user must specify
    the data kind.

    Higher-level abstractions may wish to present a uniform interface to this
    data. *)
