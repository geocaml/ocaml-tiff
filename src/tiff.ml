(* type header = {
  kind : Ifd.tiff_kind;
  byte_order : Ifd.endianness;
  offset : Optint.Int63.t;
} *)
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
