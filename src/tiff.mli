(* Declares the types and exposed functions used for interaction with the TIFF decoding API *)

(** {1 TIFF}

    This library provides functions for accessing TIFF files. A TIFF file is a
    way to provide {i raster} images. *)

open Bigarray
module File = File
module Endian = Endian
module Ifd = Ifd

type window = { xoff : int; yoff : int; xsize : int; ysize : int }
(** A window can be used to reduce the size of data returned by {! data} *)

type ('repr, 'kind) kind =
  | Uint8 : (int, int8_unsigned_elt) kind
  | Int8 : (int, int8_signed_elt) kind
  | Uint16 : (int, int16_unsigned_elt) kind
  | Int16 : (int, int16_signed_elt) kind
  | Uint32 : (int32, int32_elt) kind
  | Int32 : (int32, int32_elt) kind
  | Float32 : (float, float32_elt) kind
  | Float64 : (float, float64_elt) kind
      (** Type of data held within the TIFF *)

module Data : sig
  type ('repr, 'kind) t = ('repr, 'kind, c_layout) Genarray.t
  (** Raw TIFF data. *)
end

type ('repr, 'kind) t
(** A TIFF file *)

val from_file : ('repr, 'kind) kind -> File.ro -> ('repr, 'kind) t
(** Start reading a TIFF file with the type of data specified. *)

val to_file :
  ?plane:int option ->
  ?window:window option ->
  ('repr, 'kind) t ->
  ('repr, 'kind) Data.t ->
  File.wo ->
  unit
(** [to_file t data w] writes the tiff [t] along with the [data] to a file using
    [w].

    Note that it is up to the user to ensure the metadata in [t] is related and
    accruate with respect to [data]. *)

val ifd : ('repr, 'kind) t -> Ifd.t
(** Access the first IFD of the TIFF file *)

val ifds : ('repr, 'kind) t -> Ifd.t array
(** Access the all IFDs of the TIFF file *)

val data :
  ?image_nb:int ->
  ?plane:int ->
  ?window:window ->
  ('repr, 'kind) t ->
  File.ro ->
  ('repr, 'kind) Data.t
(** Low-level access to the raw data inside the TIFF file.

    Higher-level abstractions may wish to present a uniform interface to this
    data. *)

module Private : sig
  module Lzw = Lzw
  module Deflate = Deflate
end
