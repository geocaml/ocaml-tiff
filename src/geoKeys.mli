type t
(** An image file directory *)

type ifd = t
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
