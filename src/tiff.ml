(* Core TIFF decoder module. Handles image data extraction, decompression and window-based access using metadata from the IFD *)
open Bigarray
module File = File
module Endian = Endian
module Ifd = Ifd
module Error = Error

type header = Ifd.header

type ('repr, 'kind) kind =
  | Uint8 : (int, Bigarray.int8_unsigned_elt) kind
  | Int8 : (int, Bigarray.int8_signed_elt) kind
  | Uint16 : (int, Bigarray.int16_unsigned_elt) kind
  | Int16 : (int, Bigarray.int16_signed_elt) kind
  | Uint32 : (int32, Bigarray.int32_elt) kind
  | Int32 : (int32, Bigarray.int32_elt) kind
  | Int64 : (int64, Bigarray.int64_elt) kind
  | Float32 : (float, Bigarray.float32_elt) kind
  | Float64 : (float, Bigarray.float64_elt) kind

type ('repr, 'kind) intent =
  | As of ('repr, 'kind) kind
  | Safe_cast of ('repr, 'kind) kind
  | Unsafe_cast of ('repr, 'kind) kind

let kind_of_intent (type r k) : (r, k) intent -> (r, k) kind = function
  | As k | Safe_cast k | Unsafe_cast k -> k

let as_kind k = As k
let safe_cast k = Safe_cast k
let unsafe_cast k = Unsafe_cast k

let pp_kind (type r k) : (r, k) kind Fmt.t =
 fun ppf -> function
  | Uint8 -> Fmt.string ppf "uint8"
  | Int8 -> Fmt.string ppf "int8"
  | Uint16 -> Fmt.string ppf "uint16"
  | Int16 -> Fmt.string ppf "int16"
  | Int32 -> Fmt.string ppf "int32"
  | Uint32 -> Fmt.string ppf "uint32"
  | Int64 -> Fmt.string ppf "int64"
  | Float32 -> Fmt.string ppf "float32"
  | Float64 -> Fmt.string ppf "float64"

(* LibTIFF defines this to be 8KB (see https://libtiff.gitlab.io/libtiff/functions/TIFFstrip.html)
   but modern libraries override this and default to  64KB or higher for modern hardware. *)
let default_strip_size = 65_536

type window = { xoff : int; yoff : int; xsize : int; ysize : int }

module Data = struct
  (* open Bigarray *)

  type ('repr, 'kind) t = ('repr, 'kind, c_layout) Genarray.t

  let read_uint8_value buf buf_index _ = Cstruct.get_uint8 buf buf_index

  let read_int8_value buf buf_index _ =
    (Cstruct.get_uint8 buf buf_index lsl (Sys.int_size - 8))
    asr (Sys.int_size - 8)

  let read_uint16_value buf buf_index tiff_endianness =
    Endian.uint16 ~offset:buf_index tiff_endianness buf

  let read_int16_value buf buf_index tiff_endianness =
    Endian.int16 ~offset:buf_index tiff_endianness buf

  let _read_uint32_value buf buf_index tiff_endianness =
    Endian.uint32 ~offset:buf_index tiff_endianness buf

  let read_int32_value buf buf_index tiff_endianness =
    Endian.uint32 ~offset:buf_index tiff_endianness buf

  let read_int64_value buf buf_index tiff_endianness =
    Endian.uint64 ~offset:buf_index tiff_endianness buf

  let read_float32_value buf buf_index tiff_endianness =
    let int_value = Endian.uint32 ~offset:buf_index tiff_endianness buf in
    Int32.float_of_bits int_value

  let read_float64_value buf buf_index tiff_endianness =
    let int_value = Endian.uint64 ~offset:buf_index tiff_endianness buf in
    Int64.float_of_bits int_value

  let write_uint8_value buf buf_index value _ =
    Cstruct.set_uint8 buf buf_index value

  let write_int8_value buf buf_index value _ =
    Cstruct.set_uint8 buf buf_index (value land ((1 lsl 8) - 1))

  let write_uint16_value buf buf_index value tiff_endianness =
    Endian.set_uint16 ~offset:buf_index tiff_endianness buf value

  let write_int16_value buf buf_index value tiff_endianness =
    Endian.set_int16 ~offset:buf_index tiff_endianness buf value

  let write_uint32_value buf buf_index value tiff_endianness =
    Endian.set_uint32 ~offset:buf_index tiff_endianness buf value

  let write_int64_value buf buf_index value tiff_endianness =
    Endian.set_uint64 ~offset:buf_index tiff_endianness buf value

  let write_float32_value buf buf_index value tiff_endianness =
    Endian.set_float32 ~offset:buf_index tiff_endianness buf value

  let write_float64_value buf buf_index value tiff_endianness =
    Endian.set_double ~offset:buf_index tiff_endianness buf value

  let pp_window fmt window =
    Fmt.pf fmt "{ xoff=%i, yoff=%i, xsize=%i, ysize=%i }" window.xoff
      window.yoff window.xsize window.ysize

  let get_window ifd window =
    (* Check the window, if any, makes sense for the TIFF file *)
    let () =
      match window with
      | None -> ()
      | Some window ->
          let w = Ifd.width ifd and h = Ifd.height ifd in
          let win_w = window.xoff + window.xsize in
          let win_h = window.yoff + window.ysize in
          if Int.compare win_w w > 0 || Int.compare win_h h > 0 then
            Error.raisef Error.Invalid_argument
              "Invalid TIFF data window %a: image dimensions are %i x %i"
              pp_window window w h
    in
    match window with
    | Some w -> w
    | None ->
        let width = Ifd.width ifd in
        let height = Ifd.height ifd in
        { xoff = 0; yoff = 0; xsize = width; ysize = height }

  let get_plane ifd plane =
    let planar_configuration = Ifd.planar_configuration ifd in
    match (planar_configuration, plane) with
    | Planar, Some p -> p
    | Planar, None ->
        Error.raise_error Error.Invalid_argument
          "Cannot read planar TIFF data: a plane index must be specified"
    | Chunky, None | Chunky, Some 0 -> 0
    | Chunky, Some _ ->
        Error.raise_error Error.Invalid_argument
          "Can not select plane on single plane TIFFs"
    | Unknown _, _ ->
        Error.raise_error Error.Malformed_file "Unknown planar format TIFF"

  let process_data ifd (header : Ifd.header) plane window ~on_strip ~on_value
      ~after_strip =
    let height = Ifd.height ifd in
    let width = Ifd.width ifd in
    let samples_per_pixel = Ifd.samples_per_pixel ifd in
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
    let tiff_endianness = header.byte_order in
    let strip_offsets_length = Array.length strip_offsets in
    if strip_offsets_length <> Array.length strip_bytecounts then
      raise
        (Invalid_argument
           "strip_offsets and strip_bytecounts are of different lengths");
    let strips_per_plane =
      match planar_configuration with
      | Planar -> Array.length strip_offsets / List.length bits_per_sample
      | Chunky -> Array.length strip_offsets
      | Unknown _ ->
          failwith "should not get this far if planar config isn't recognised"
    in

    let plane_offset = strips_per_plane * plane in

    (* Find the first and last strips touched by the requested window.
       Divide the first and last row indices by rows_per_strip to get
       their strip indices, then add the plane offset. *)
    let first_strip = (window.yoff / rows_per_strip) + plane_offset in
    let last_strip =
      ((window.yoff + window.ysize - 1) / rows_per_strip) + plane_offset
    in

    let row_index = ref ((first_strip - plane_offset) * rows_per_strip) in

    (* Iterate over every strip touched by the window, including both endpoints. *)
    for strip = first_strip to last_strip do
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

      (* Iterating through the rows of the current strip *)
      let strip_buffer =
        on_strip expected_size raw_strip_buffer sparse strip_offset
      in

      for inner_row = 0 to rows_in_strip - 1 do
        (* The actual y offset we are at *)
        let y_offset = !row_index + inner_row in

        if y_offset >= window.yoff + window.ysize || y_offset < window.yoff then
          ()
        else
          (* The x offset we are at *)
          let x_offset = ref window.xoff in

          while !x_offset <= window.xoff + window.xsize - 1 do
            (* The index into the strip buffer *)
            let index = !x_offset + (inner_row * width) in

            if samples_per_pixel = 1 || planar_configuration = Planar then
              on_value strip_buffer (index * bytes_per_pixel)
                [| y_offset - window.yoff; !x_offset - window.xoff |]
                tiff_endianness
            else
              for channel = 0 to samples_per_pixel - 1 do
                (*Calculate offset for each byte in the pixel*)
                let byte_off =
                  bytes_per_pixel / List.length bits_per_sample * channel
                in

                on_value strip_buffer
                  ((index * bytes_per_pixel) + byte_off)
                  [| y_offset - window.yoff; !x_offset - window.xoff; channel |]
                  tiff_endianness
              done;
            x_offset := !x_offset + 1
          done
      done;
      after_strip strip_buffer strip_offset;
      row_index := !row_index + rows_in_strip
    done

  (* Turn horizontal differences back into pixels. 8- or 16-bit samples. *)
  let undo_horizontal_differencing buf ~width ~samples_per_pixel
      ~bytes_per_sample ~endian =
    let samples_per_row = width * samples_per_pixel in
    let bytes_per_row = samples_per_row * bytes_per_sample in
    let rows = Cstruct.length buf / bytes_per_row in
    (* Differences are stored modulo 2^bits (TIFF 6, Section 14).
       Masking wraps sums that exceed the maximum sample value,
       e.g. (250 + 10) mod 256 = 4 for 8-bit samples. *)
    let mask = (1 lsl (8 * bytes_per_sample)) - 1 in
    let get position =
      let offset = position * bytes_per_sample in
      if bytes_per_sample = 1 then Cstruct.get_uint8 buf offset
      else Endian.uint16 ~offset endian buf
    in
    let set position value =
      let offset = position * bytes_per_sample in
      if bytes_per_sample = 1 then Cstruct.set_uint8 buf offset value
      else Endian.set_uint16 ~offset endian buf value
    in
    for r = 0 to rows - 1 do
      let row_start = r * samples_per_row in
      (* The first pixel has no left neighbour and is stored as-is.
         Differencing starts at the second pixel, at samples_per_pixel. *)
      for i = samples_per_pixel to samples_per_row - 1 do
        let position = row_start + i in
        let left = get (position - samples_per_pixel) in
        let value = (get position + left) land mask in
        set position value
      done
    done

  let apply_predictor ifd (header : Ifd.header) planar_configuration
      samples_per_pixel buf =
    match Ifd.predictor ifd with
    | Ifd.No_predictor -> ()
    | Ifd.HorizontalDifferencing ->
        let samples =
          match planar_configuration with
          | Ifd.Chunky -> samples_per_pixel
          | Ifd.Planar -> 1
          | Ifd.Unknown _ ->
              failwith
                "Should not get this far if planar config isn't recognised."
        in
        let bits_per_sample = List.hd (Ifd.bits_per_sample ifd) in
        if bits_per_sample <> 8 && bits_per_sample <> 16 then
          failwith
            (Printf.sprintf
               "Unsupported bits per sample for horizontal differencing: %d \
                (only 8 and 16 are supported)"
               bits_per_sample);
        let bytes_per_sample = bits_per_sample / 8 in
        undo_horizontal_differencing buf ~width:(Ifd.width ifd)
          ~samples_per_pixel:samples ~bytes_per_sample ~endian:header.byte_order
    | Ifd.Unknown n ->
        Error.raisef Error.Unsupported_feature "Unsupported predictor %d" n

  let read_data ifd header ro plane window arr_type read_value =
    let samples_per_pixel = Ifd.samples_per_pixel ifd in
    let planar_configuration = Ifd.planar_configuration ifd in

    let arr =
      if samples_per_pixel = 1 || planar_configuration = Planar then
        Genarray.create arr_type c_layout [| window.ysize; window.xsize |]
      else
        Genarray.create arr_type c_layout
          [| window.ysize; window.xsize; samples_per_pixel |]
    in
    process_data ifd header plane window
      ~on_strip:(fun expected_size buf sparse strip_offset ->
        if not sparse then ro ~file_offset:strip_offset [ buf ];

        match (sparse, Ifd.compression ifd) with
        | true, _ | false, No_compression ->
            if Cstruct.length buf < expected_size then
              Error.raise_error Error.Malformed_file
                "Strip is unexpectedly short";
            buf
        | false, LZW ->
            let uncompressed_buffer = Cstruct.create expected_size in
            Lzw.decode buf uncompressed_buffer;
            apply_predictor ifd header planar_configuration samples_per_pixel
              uncompressed_buffer;
            uncompressed_buffer
        | false, DEFLATE | false, ADOBE_DEFLATE ->
            let uncompressed_buffer = Cstruct.create expected_size in
            Deflate.decode buf uncompressed_buffer;
            apply_predictor ifd header planar_configuration samples_per_pixel
              uncompressed_buffer;
            uncompressed_buffer
        | _ ->
            Error.raise_error Error.Unsupported_feature
              "Unsupported compression")
      ~on_value:(fun buf buf_offset dim endian ->
        let value = read_value buf buf_offset endian in
        Genarray.set arr dim value)
      ~after_strip:(fun _ _ -> ());
    arr

  let write_data ifd header plane window data w write_value =
    process_data ifd header plane window
      ~on_strip:(fun expected_size buf sparse _ ->
        match (sparse, Ifd.compression ifd) with
        | true, _ | false, No_compression ->
            if Cstruct.length buf < expected_size then
              Error.raise_error Error.Unsupported_feature
                "Strip is unexpectedly short";
            buf
        | _ ->
            Error.raise_error Error.Unsupported_feature
              "Unsupported compression")
      ~on_value:(fun buf buf_offset dim tiff_endianness ->
        let value = Genarray.get data dim in
        write_value buf buf_offset value tiff_endianness)
      ~after_strip:(fun buf strip_offset -> w ~file_offset:strip_offset [ buf ])

  let bits_per_sample_of_kind (type a b) (kind : (a, b) Bigarray.kind) =
    match kind with
    | Bigarray.Int8_unsigned -> (8, Ifd.UnsignedInteger)
    | Bigarray.Int8_signed -> (8, Ifd.SignedInteger)
    | Bigarray.Int16_signed -> (16, Ifd.SignedInteger)
    | Bigarray.Int16_unsigned -> (16, Ifd.UnsignedInteger)
    | Bigarray.Int32 -> (32, Ifd.SignedInteger)
    | Bigarray.Int64 -> (64, Ifd.SignedInteger)
    | Bigarray.Float32 -> (32, Ifd.IEEEFloatingPoint)
    | Bigarray.Float64 -> (64, Ifd.IEEEFloatingPoint)
    | _ -> failwith "Unsupported element kind"

  (* The rows_per_strip is calculated based on trying to cram as many rows into
     a 64KB strip, we may wish to allow a user to override this in the future.

     Note this independent of image height. *)
  let compute_rows_per_strip ~bits_per_sample ~samples_per_pixel ~width =
    let bytes_per_row = width * bits_per_sample * samples_per_pixel / 8 in
    let rows_per_strip = max 1 (default_strip_size / bytes_per_row) in
    (rows_per_strip, bytes_per_row)

  (* Total byte counts per strip, must strips will be full (i.e. have [rows_per_strip] rows
     in them). Only the last might be short. *)
  let compute_data_bytecounts ~rows_per_strip ~height ~bytes_per_row =
    let num_strips = height / rows_per_strip in
    let rem_strips = height mod rows_per_strip in
    let full_strips =
      List.init num_strips (fun _ -> rows_per_strip * bytes_per_row)
    in
    if rem_strips = 0 then full_strips
    else full_strips @ [ rem_strips * bytes_per_row ]

  (* We just do the naive thing for now and dump all of the data together. *)
  let compute_data_offsets strip_bytecounts start_offset =
    let _, data_offsets =
      List.fold_left
        (fun (current_offset, acc) count ->
          (current_offset + count, current_offset :: acc))
        (start_offset, []) strip_bytecounts
    in
    let data_offsets = List.rev data_offsets in
    data_offsets
end

type ('repr, 'kind) t = {
  intent : ('repr, 'kind) intent;
  mutable window : window option;
  mutable plane : int option;
  mutable image_nb : int option;
  mutable data : ('repr, 'kind) Data.t option;
  header : header;
  ifds : Ifd.t array;
}

let ifd t = t.ifds.(0)
let ifds t = t.ifds

let all_ifds first_ifd =
  let rec aux acc ifd =
    match Ifd.next_ifd ifd with
    | None -> acc
    | Some ifd' -> aux (ifd' :: acc) ifd'
  in
  aux [ first_ifd ] first_ifd |> List.rev |> Array.of_list

let from_file (type a b) (intent : (a, b) intent) (f : File.ro) : (a, b) t =
  let header = Ifd.read_header f in
  let first_ifd = Ifd.v ~file_offset:header.offset header f in
  let ifds = all_ifds first_ifd in
  {
    intent;
    header;
    window = None;
    plane = None;
    image_nb = None;
    data = None;
    ifds;
  }

let bigarray_kind (type r k) : (r, k) kind -> (r, k) Bigarray.kind = function
  | Uint8 -> Bigarray.int8_unsigned
  | Int8 -> Bigarray.int8_signed
  | Uint16 -> Bigarray.int16_unsigned
  | Int16 -> Bigarray.int16_signed
  | Uint32 -> Bigarray.int32
  | Int32 -> Bigarray.int32
  | Int64 -> Bigarray.int64
  | Float32 -> Bigarray.float32
  | Float64 -> Bigarray.float64

let integer_bounds (fmt : Ifd.sample_format) bpp =
  match (fmt, bpp) with
  | UnsignedInteger, 8 -> Some (0L, 255L)
  | SignedInteger, 8 -> Some (-128L, 127L)
  | UnsignedInteger, 16 -> Some (0L, 65535L)
  | SignedInteger, 16 -> Some (-32768L, 32767L)
  | UnsignedInteger, 32 -> Some (0L, 0xffffffffL)
  | SignedInteger, 32 ->
      Some (Int64.of_int32 Int32.min_int, Int64.of_int32 Int32.max_int)
  | SignedInteger, 64 -> Some (Int64.min_int, Int64.max_int)
  | _ -> None

let holds (type r k) (kind : (r, k) kind) lo hi =
  let ok a b = Int64.compare lo a >= 0 && Int64.compare hi b <= 0 in
  match kind with
  | Uint8 -> ok 0L 255L
  | Int8 -> ok (-128L) 127L
  | Uint16 -> ok 0L 65535L
  | Int16 -> ok (-32768L) 32767L
  | Uint32 -> ok 0L (Int64.of_int32 Int32.max_int)
  | Int32 -> ok (Int64.of_int32 Int32.min_int) (Int64.of_int32 Int32.max_int)
  | Int64 -> ok Int64.min_int Int64.max_int
  | Float32 -> ok (Int64.neg (Int64.shift_left 1L 24)) (Int64.shift_left 1L 24)
  | Float64 -> ok (Int64.neg (Int64.shift_left 1L 53)) (Int64.shift_left 1L 53)

let narrow (type r k) (kind : (r, k) kind) n : r =
  match kind with
  | Uint8 -> Int64.to_int n
  | Int8 -> Int64.to_int n
  | Uint16 -> Int64.to_int n
  | Int16 -> Int64.to_int n
  | Uint32 -> Int64.to_int32 n
  | Int32 -> Int64.to_int32 n
  | Int64 -> n
  | Float32 -> Int64.to_float n
  | Float64 -> Int64.to_float n

let read_int64 (fmt : Ifd.sample_format) bpp buf off endian =
  match (fmt, bpp) with
  | UnsignedInteger, 8 -> Int64.of_int (Data.read_uint8_value buf off endian)
  | SignedInteger, 8 -> Int64.of_int (Data.read_int8_value buf off endian)
  | UnsignedInteger, 16 -> Int64.of_int (Data.read_uint16_value buf off endian)
  | SignedInteger, 16 -> Int64.of_int (Data.read_int16_value buf off endian)
  | UnsignedInteger, 32 ->
      Int64.logand
        (Int64.of_int32 (Endian.uint32 ~offset:off endian buf))
        0xffffffffL
  | SignedInteger, 32 -> Int64.of_int32 (Data.read_int32_value buf off endian)
  | SignedInteger, 64 -> Data.read_int64_value buf off endian
  | _ -> assert false

let exact_io (type r k) (kind : (r, k) kind) (fmt : Ifd.sample_format) bpp :
    ((Cstruct.t -> int -> Endian.endianness -> r)
    * (Cstruct.t -> int -> r -> Endian.endianness -> unit))
    option =
  match (kind, fmt, bpp) with
  | Uint8, UnsignedInteger, 8 ->
      Some (Data.read_uint8_value, Data.write_uint8_value)
  | Int8, SignedInteger, 8 -> Some (Data.read_int8_value, Data.write_int8_value)
  | Uint16, UnsignedInteger, 16 ->
      Some (Data.read_uint16_value, Data.write_uint16_value)
  | Int16, SignedInteger, 16 ->
      Some (Data.read_int16_value, Data.write_int16_value)
  | Int32, SignedInteger, 32 ->
      Some (Data.read_int32_value, Data.write_uint32_value)
  | Int64, SignedInteger, 64 ->
      Some (Data.read_int64_value, Data.write_int64_value)
  | Float32, IEEEFloatingPoint, 32 ->
      Some (Data.read_float32_value, Data.write_float32_value)
  | Float64, IEEEFloatingPoint, 64 ->
      Some (Data.read_float64_value, Data.write_float64_value)
  | _ -> None

let datatype_mismatch kind (fmt : Ifd.sample_format) bpp =
  Error.raisef Error.Invalid_argument
    "datatype not correct for plane: %a, %a, %i bpp" pp_kind kind
    Ifd.pp_sample_format fmt bpp

let would_lose_precision kind (fmt : Ifd.sample_format) bpp =
  Error.raisef Error.Invalid_argument
    "safe cast to %a would lose data: %a, %i bpp" pp_kind kind
    Ifd.pp_sample_format fmt bpp

let exact_repr (type r k) (kind : (r, k) kind) (fmt : Ifd.sample_format) bpp =
  match kind with
  | Uint32 when fmt = UnsignedInteger && bpp = 32 ->
      Error.raisef Error.Unsupported_feature
        "Unsupported datatype for plane: %a, %a, %i bpp" pp_kind kind
        Ifd.pp_sample_format fmt bpp
  | _ -> (
      match exact_io kind fmt bpp with
      | Some (read, write) -> (bigarray_kind kind, read, write)
      | None -> datatype_mismatch kind fmt bpp)

let converted_reader (type r k) ~unsafe_ok (kind : (r, k) kind)
    (fmt : Ifd.sample_format) bpp =
  let src =
    match integer_bounds fmt bpp with
    | Some (lo, hi) ->
        if (not unsafe_ok) && not (holds kind lo hi) then
          would_lose_precision kind fmt bpp;
        `Int
    | None when fmt = IEEEFloatingPoint && (bpp = 32 || bpp = 64) ->
        let safe =
          match kind with Float64 -> true | Float32 -> bpp = 32 | _ -> false
        in
        if (not unsafe_ok) && not safe then would_lose_precision kind fmt bpp;
        `Float
    | None -> datatype_mismatch kind fmt bpp
  in
  let read buf off endian =
    match src with
    (* Int64 is a convenient intermediate. Direct conversions may be faster;
       worth revisiting if casting shows up in profiles. *)
    | `Int -> narrow kind (read_int64 fmt bpp buf off endian)
    | `Float -> (
        let f =
          if bpp = 32 then Data.read_float32_value buf off endian
          else Data.read_float64_value buf off endian
        in
        match kind with
        | Float32 -> f
        | Float64 -> f
        | _ -> narrow kind (Int64.of_float f))
  in
  (bigarray_kind kind, read)

let read_repr (type r k) (t : (r, k) t) ifd plane =
  let fmt = Ifd.sample_format ifd in
  let bpp = List.nth (Ifd.bits_per_sample ifd) plane in
  let kind = kind_of_intent t.intent in
  match t.intent with
  | As _ ->
      let ba, read, _ = exact_repr kind fmt bpp in
      (ba, read)
  | Safe_cast _ -> (
      match exact_io kind fmt bpp with
      | Some (read, _) -> (bigarray_kind kind, read)
      | None -> converted_reader ~unsafe_ok:false kind fmt bpp)
  | Unsafe_cast _ -> (
      match exact_io kind fmt bpp with
      | Some (read, _) -> (bigarray_kind kind, read)
      | None -> converted_reader ~unsafe_ok:true kind fmt bpp)

let data (type repr kind) ?(image_nb = 0) ?plane ?window (t : (repr, kind) t)
    (f : File.ro) : (repr, kind) Data.t =
  let nb_images = Array.length t.ifds in
  if image_nb < 0 || image_nb >= nb_images then
    Error.raisef Error.Invalid_argument
      "Image n°%d requested but TIFF file only has %d ([0-%d])" image_nb
      nb_images (nb_images - 1);
  let ifd = t.ifds.(image_nb) in
  let window = Data.get_window ifd window in
  let plane = Data.get_plane ifd plane in
  match t.data with
  | Some data
    when t.window = Some window && t.plane = Some plane
         && t.image_nb = Some image_nb ->
      data
  | Some _ | None ->
      let kind, read_value = read_repr t ifd plane in
      let data = Data.read_data ifd t.header f plane window kind read_value in
      t.window <- Some window;
      t.plane <- Some plane;
      t.image_nb <- Some image_nb;
      t.data <- Some data;
      data

let write_data (type repr kind) ?(plane = None) ?(window = None) tiff ifd
    (data : (repr, kind) Data.t) (w : File.wo) : _ =
  let window = Data.get_window ifd window in
  let plane = Data.get_plane ifd plane in
  let _, _, write_value =
    exact_repr
      (kind_of_intent tiff.intent)
      (Ifd.sample_format ifd)
      (List.nth (Ifd.bits_per_sample ifd) plane)
  in
  Data.write_data ifd tiff.header plane window data w write_value

let to_file (type repr kind) ?plane ?window (tiff : (repr, kind) t)
    (w : File.wo) =
  let ifd = Ifd.cache_all_entries (ifd tiff) in
  Ifd.write_header w tiff.header;
  Ifd.write_ifd ~file_offset:tiff.header.offset tiff.header w ifd;
  match tiff.data with
  | None ->
      Error.raise_error Error.Malformed_file
        "Tiff file has no data associated with it."
  | Some data -> write_data ~plane ~window tiff ifd data w

let make ?(bigtiff = false) ?(endian = Endian.Big)
    ?(compression = Ifd.No_compression) ?(photometric_interpretation = Ifd.RGB)
    ?(planar_configuration = Ifd.Chunky) ?(file_name = "TIFF_File") data =
  let header = Ifd.create_header ~bigtiff endian in
  let ifd_count, ifd_entries, next_ifd_ptr =
    match header.kind with Tiff -> (2, 12, 4) | Bigtiff -> (8, 20, 8)
  in
  let compression_int = Ifd.compression_to_int compression in
  let planar_int = Ifd.planar_configuration_to_int planar_configuration in
  let photometric_int =
    Ifd.photometric_interpretation_to_int photometric_interpretation
  in
  let number_of_entries = 12 in
  let ifd_size = ifd_count + (number_of_entries * ifd_entries) + next_ifd_ptr in
  let extra_data_offset = (header.offset |> Optint.Int63.to_int) + ifd_size in
  let file_offset = extra_data_offset in
  let num_dims = Genarray.num_dims data in

  let samples_per_pixel =
    if num_dims > 2 then Genarray.nth_dim data (num_dims - 1) else 1
  in

  let width = Genarray.nth_dim data 1 in
  let height = Genarray.nth_dim data 0 in

  let bits_per_sample, sample_format =
    Data.bits_per_sample_of_kind (Genarray.kind data)
  in
  let sample_format_int = Ifd.sample_format_to_int sample_format in
  let bps_list = List.init samples_per_pixel (fun _ -> bits_per_sample) in

  let rows_per_strip, bytes_per_row =
    Data.compute_rows_per_strip ~bits_per_sample ~samples_per_pixel ~width
  in

  let data_bytecounts =
    Data.compute_data_bytecounts ~rows_per_strip ~height ~bytes_per_row
  in

  let make_entry = Ifd.make_entry endian in
  let image_width, file_offset =
    make_entry file_offset ImageWidth (Ifd.Ints [ width ])
  in
  let image_height, file_offset =
    make_entry file_offset ImageLength (Ifd.Ints [ height ])
  in
  let bits_per_sample, file_offset =
    make_entry file_offset BitsPerSample (Ifd.Ints bps_list)
  in
  let compression, file_offset =
    make_entry file_offset Compression (Ifd.Ints [ compression_int ])
  in
  let photometric_interpretation, file_offset =
    make_entry file_offset PhotometricInterpretation
      (Ifd.Ints [ photometric_int ])
  in
  let planar_config, file_offset =
    make_entry file_offset PlanarConfiguration (Ifd.Ints [ planar_int ])
  in
  let rows_per_strip, file_offset =
    make_entry file_offset RowsPerStrip (Ifd.Ints [ rows_per_strip ])
  in

  let strip_bytecounts, file_offset =
    make_entry file_offset StripByteCounts (Ifd.Ints data_bytecounts)
  in
  let samples_per_pixel, file_offset =
    make_entry file_offset SamplesPerPixel (Ifd.Ints [ samples_per_pixel ])
  in
  let sample_format, file_offset =
    make_entry file_offset SampleFormat (Ifd.Ints [ sample_format_int ])
  in

  let document_name, file_offset =
    make_entry file_offset DocumentName (Ifd.String file_name)
  in
  let num_strips = List.length data_bytecounts in
  let data_offsets, start_image_data_offset, array_element_size =
    if num_strips = 1 then ([ file_offset ], file_offset, 0)
    else
      let total_image_bytes = bytes_per_row * height in
      let short_array_size = num_strips * 2 in
      let last_offset_if_short =
        file_offset + short_array_size + total_image_bytes
        - List.hd (List.rev data_bytecounts)
      in
      if last_offset_if_short <= 65535 then
        let start_offset = file_offset + short_array_size in
        let offsets = Data.compute_data_offsets data_bytecounts start_offset in
        (offsets, start_offset, 2)
      else
        let long_array_size = num_strips * 4 in
        let start_offset = file_offset + long_array_size in
        let offsets = Data.compute_data_offsets data_bytecounts start_offset in
        (offsets, start_offset, 4)
  in

  let strip_offset, _ =
    make_entry
      (start_image_data_offset - (num_strips * array_element_size))
      StripOffsets (Ifd.Ints data_offsets)
  in

  let of_ba_kind (type r k) : (r, k) Bigarray.kind -> (r, k) kind = function
    | Int8_signed -> Int8
    | Int8_unsigned -> Uint8
    | Int16_signed -> Int16
    | Int16_unsigned -> Uint16
    | Int32 -> Int32
    | Int64 -> Int64
    | Float32 -> Float32
    | Float64 -> Float64
    | k ->
        Error.raisef Error.Unsupported_feature "Converting failed for kind %i"
          (Bigarray.kind_size_in_bytes k)
  in
  let entries =
    [
      image_width;
      image_height;
      bits_per_sample;
      compression;
      photometric_interpretation;
      document_name;
      strip_offset;
      samples_per_pixel;
      rows_per_strip;
      strip_bytecounts;
      planar_config;
      sample_format;
    ]
  in
  let ro = fun ~file_offset:_ _ -> Fmt.failwith "No file" in
  let data_type = of_ba_kind (Bigarray.Genarray.kind data) in
  let ifd = Ifd.v_of_entries entries data_offsets data_bytecounts header ro in
  {
    intent = As data_type;
    data = Some data;
    window = None;
    plane = None;
    image_nb = None;
    header;
    ifds = Array.make 1 ifd;
  }

module Private = struct
  module Lzw = Lzw
  module Deflate = Deflate
end
