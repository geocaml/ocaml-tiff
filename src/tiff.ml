(* Core TIFF decoder module. Handles image data extraction, decompression and window-based access using metadata from the IFD *)
open Bigarray
module File = File
module Endian = Endian
module Ifd = Ifd

type header = Ifd.header

type ('repr, 'kind) kind =
  | Uint8 : (int, Bigarray.int8_unsigned_elt) kind
  | Int8 : (int, Bigarray.int8_signed_elt) kind
  | Uint16 : (int, Bigarray.int16_unsigned_elt) kind
  | Int16 : (int, Bigarray.int16_signed_elt) kind
  | Uint32 : (int32, Bigarray.int32_elt) kind
  | Int32 : (int32, Bigarray.int32_elt) kind
  | Float32 : (float, Bigarray.float32_elt) kind
  | Float64 : (float, Bigarray.float64_elt) kind

let pp_kind (type r k) : (r, k) kind Fmt.t =
 fun ppf -> function
  | Uint8 -> Fmt.string ppf "uint8"
  | Int8 -> Fmt.string ppf "int8"
  | Uint16 -> Fmt.string ppf "uint16"
  | Int16 -> Fmt.string ppf "int16"
  | Int32 -> Fmt.string ppf "int32"
  | Uint32 -> Fmt.string ppf "uint32"
  | Float32 -> Fmt.string ppf "float32"
  | Float64 -> Fmt.string ppf "float64"

type ('repr, 'kind) t = {
  data_type : ('repr, 'kind) kind;
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

let from_file (type a b) (data_type : (a, b) kind) (f : File.ro) : (a, b) t =
  let header = Ifd.read_header f in
  let first_ifd = Ifd.v ~file_offset:header.offset header f in
  let ifds = all_ifds first_ifd in
  { data_type; header; ifds }

type window = { xoff : int; yoff : int; xsize : int; ysize : int }

module Data = struct
  (* open Bigarray *)

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

  let _read_uint32_value buf buf_index tiff_endianness =
    Endian.uint32 ~offset:buf_index tiff_endianness buf

  let read_int32_value buf buf_index tiff_endianness =
    Endian.uint32 ~offset:buf_index tiff_endianness buf

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

  let write_float32_value buf buf_index value tiff_endianness =
    Endian.set_float32 ~offset:buf_index tiff_endianness buf value

  let write_float64_value buf buf_index value tiff_endianness =
    Endian.set_double ~offset:buf_index tiff_endianness buf value

  let ceil a b = (a + b - 1) / b

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
            Fmt.invalid_arg "Window %a for data (%i %i)" pp_window window w h
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
    | Planar, None -> invalid_arg "Must specify plane for data read"
    | Chunky, None | Chunky, Some 0 -> 0
    | Chunky, Some _ -> invalid_arg "Can not select plane on single plane TIFFs"
    | Unknown _, _ -> invalid_arg "Unknown planar format TIFF"

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

    let first_strip =
      (window.yoff / rows_per_strip) + (strips_per_plane * plane)
    in
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
              failwith "Strip is unexpectedly short";
            buf
        | false, LZW ->
            let uncompressed_buffer = Cstruct.create expected_size in
            Lzw.decode buf uncompressed_buffer;
            uncompressed_buffer
        | false, DEFLATE | false, ADOBE_DEFLATE ->
            let uncompressed_buffer = Cstruct.create expected_size in
            Deflate.decode buf uncompressed_buffer;
            uncompressed_buffer
        | _ -> failwith "Unsupported compression")
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
              failwith "Strip is unexpectedly short";
            buf
        | _ -> failwith "compression coming soon")
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
    | Bigarray.Int32 -> (32, Ifd.UnsignedInteger)
    | Bigarray.Float32 -> (32, Ifd.IEEEFloatingPoint)
    | Bigarray.Float64 -> (64, Ifd.IEEEFloatingPoint)
    | _ -> failwith "Unsupported element kind"
end

let get_repr (type repr kind) (t : (repr, kind) t) ifd plane :
    (repr, kind) Bigarray.kind
    * (Cstruct.t -> int -> Endian.endianness -> repr)
    * (Cstruct.t -> int -> repr -> Endian.endianness -> unit) =
  let sample_format = Ifd.sample_format ifd in
  let bpp = List.nth (Ifd.bits_per_sample ifd) plane in

  match (t.data_type, sample_format, bpp) with
  | Uint8, UnsignedInteger, 8 ->
      (Bigarray.int8_unsigned, Data.read_uint8_value, Data.write_uint8_value)
  | Int8, SignedInteger, 8 ->
      (Bigarray.int8_signed, Data.read_int8_value, Data.write_int8_value)
  | Uint16, UnsignedInteger, 16 ->
      (Bigarray.int16_unsigned, Data.read_uint16_value, Data.write_uint16_value)
  | Int16, SignedInteger, 16 ->
      (Bigarray.int16_signed, Data.read_int16_value, Data.write_int16_value)
  | Uint32, UnsignedInteger, 32 ->
      Fmt.invalid_arg "Unsigned 32-bit coming soon..."
  | Int32, SignedInteger, 32 ->
      (Bigarray.int32, Data.read_int32_value, Data.write_uint32_value)
  | Float32, IEEEFloatingPoint, 32 ->
      (Bigarray.float32, Data.read_float32_value, Data.write_float32_value)
  | Float64, IEEEFloatingPoint, 64 ->
      (Bigarray.float64, Data.read_float64_value, Data.write_float64_value)
  | typ, fmt, bpp ->
      Fmt.invalid_arg "datatype not correct for plane: %a, %a, %i bpp" pp_kind
        typ Ifd.pp_sample_format fmt bpp

let data (type repr kind) ?(image_nb = 0) ?plane ?window (t : (repr, kind) t)
    (f : File.ro) : (repr, kind) Data.t =
  let nb_images = Array.length t.ifds in
  if image_nb < 0 || image_nb >= nb_images then
    Fmt.invalid_arg "Image n°%d requested but TIFF file only has %d ([0-%d])"
      image_nb nb_images (nb_images - 1);
  let ifd = t.ifds.(image_nb) in
  let window = Data.get_window ifd window in
  let plane = Data.get_plane ifd plane in

  let kind, read_value, _ = get_repr t ifd plane in
  Data.read_data ifd t.header f plane window kind read_value

let add_data (type repr kind) ?(plane = None) ?(window = None)
    (tiff : (repr, kind) t) (data : (repr, kind) Data.t) (w : File.wo) =
  let ifd = ifd tiff in
  let window = Data.get_window ifd window in
  let plane = Data.get_plane ifd plane in
  let _, _, write_value = get_repr tiff ifd plane in
  Data.write_data ifd tiff.header plane window data w write_value

let to_file (type repr kind) ?plane ?window (tiff : (repr, kind) t)
    (data : (repr, kind) Data.t) (w : File.wo) =
  Ifd.write_header w tiff.header;
  Ifd.write_ifd ~file_offset:tiff.header.offset tiff.header w (ifd tiff);
  add_data ~plane ~window tiff data w

(* 
      [header] -> 8 bytes TIFF, 16 bytes BigTIFF
      [IFD count] -> 2 bytes TIFF, 8 bytes BigTIFF
      [IFD entries] -> number_of_entries x 12 (TIFF) or x 20 (BigTIFF)
      [Next IFD ptr] -> 4 bytes TIFF, 8 bytes BigTIFF
      [extra data] -> non-immediate values
      [strip data] -> pixel data
  *)
let make (type c d) ?(big_tiff = false) ?(big_endian = false)
    ?(compression = Ifd.No_compression) ?(photometric_interpretation = Ifd.RGB)
    ?(planar_configuration = Ifd.Chunky) ?(file_name = "TIFF_File")
    (data_type : (c, d) kind) (data : ('c, 'd, 'e) Bigarray.Genarray.t)
    (w : File.wo) =
  (*for a basic TIFF file*)
  let endian = if big_endian then Endian.Big else Endian.Little in

  let header = Ifd.create_header ~big_tiff endian in
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

  let bps, sample_format = Data.bits_per_sample_of_kind (Genarray.kind data) in
  let sample_format_int = Ifd.sample_format_to_int sample_format in
  let bps_list = List.init samples_per_pixel (fun _ -> bps) in
  let data_bytecounts = [ height * width * samples_per_pixel * (bps / 8) ] in

  let rows_per_strip = height in
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
  (*single strip*)
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

  let data_offsets = [ file_offset ] in

  let strip_offset, _ =
    make_entry file_offset StripOffsets (Ifd.Ints data_offsets)
  in

  let make_entries =
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

  Ifd.write_header w header;
  let entries =
    Ifd.write_raw_ifd ~file_offset:header.offset header w make_entries
  in
  let ro = fun ~file_offset:_ _ -> () in
  let ifd = Ifd.v_of_entries entries data_offsets data_bytecounts header ro in

  { data_type; header; ifds = Array.make 1 ifd }

module Private = struct
  module Lzw = Lzw
  module Deflate = Deflate
end
