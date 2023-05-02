(** {1 TIFF}

This library provides functions for accessing TIFF files. A TIFF file
is a way to provide {i raster} images.
*)

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
    | GeoAsciiParams
    | GeoKeyDirectory
    | Unknown of int

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

  val samples_per_pixel : t -> int
  (** [samples_per_pixel] is usually 1 for grayscale and 3 for RGB. *)

  val bits_per_sample : t -> int list
  (** [bits_per_sample t] is the number of bits per component corresponding to a pixel.
      The following invariant should hold [List.length (bits_per_sample t) = sample_per_pixel t]. *)

  (** {3 GeoTIFF Specific} *)

  val pixel_scale : t -> float list
  (** Pixel scales entry. *)

  val tiepoint : t -> float list
  (** Also known as GeoreferenceTag, this stores raster to model
      tiepoint pairs. *)

  val geo_ascii_params : t -> string
  (** All of the ASCII valued GeoKeys. *)

  val geo_key_directory : t -> int list
  (** This tag may be used to store the GeoKey Directory, which defines and references the "GeoKeys" *)

  val data_offsets : t -> int list
  (** The offsets into the file for the chunks of the data. *)

  val data_bytecounts : t -> int list
  (** The number of bytes of each chunk of data. *)

  (** {2 Reading entries} *)

  val read_entry_short : entry -> int
  (** Reads the value of the entry as a short if the entry field matches
      otherwise it will raise [Invalid_argument _]. *)

  val read_entry_raw : entry -> #Eio.File.ro -> Cstruct.t list
  (** Read entries as raw bytes. This will return a buffer list based on the
      layout of the entry. For example, if the count is [10] and the tag is
      [Double] then you will get back a list of [10] buffers each of length [2]. *)
end

type _ t
(** A TIFF file *)

val ifd : Eio.File.ro t -> Ifd.t
(** Access the IFD of the TIFF file *)

val from_file : Eio.File.ro -> Eio.File.ro t
(** Start reading a TIFF file *)
