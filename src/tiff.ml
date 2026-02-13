(* Core TIFF decoder module. Handles image data extraction, decompression and window-based access using metadata from the IFD *)

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
  | Uint8 -> Fmt.string ppf "unit8"
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
  ifd : Ifd.t;
}

let ifd t = t.ifd

let from_file (type a b) (data_type : (a, b) kind) (f : File.ro) : (a, b) t =
  let header = Ifd.read_header f in
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

  let get_window ifd window =
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
    | Chunky, Some _ -> invalid_arg "Can not select plane on single TIFFs"
    | Unknown _, _ -> invalid_arg "Unknown planar format TIFF"

  (* Apply horizontal differencing predictor (TIFF predictor=2).
     Each row: first pixel is absolute, subsequent pixels are deltas.
     bytes_per_sample is the size of each sample component.
     row_bytes is the number of bytes per row.
     samples_per_pixel is the number of components per pixel. *)
  let apply_horizontal_differencing buf ~row_bytes ~bytes_per_sample
      ~samples_per_pixel ~num_rows =
    for row = 0 to num_rows - 1 do
      let row_off = row * row_bytes in
      (* Start from the second pixel (first pixel is absolute) *)
      for i = samples_per_pixel * bytes_per_sample to row_bytes - 1 do
        let prev =
          Cstruct.get_uint8 buf
            (row_off + i - (samples_per_pixel * bytes_per_sample))
        in
        let cur = Cstruct.get_uint8 buf (row_off + i) in
        Cstruct.set_uint8 buf (row_off + i) ((prev + cur) land 0xff)
      done
    done

  let make_set_pixel ~samples_per_pixel ~planar_configuration ~bytes_per_pixel
      ~bits_per_sample ~tiff_endianness read_value =
    let bytes_per_channel = bytes_per_pixel / List.length bits_per_sample in
    fun arr buf buf_idx y x ->
      if samples_per_pixel = 1 || planar_configuration = Ifd.Planar then
        Genarray.set arr [| y; x |] (read_value buf buf_idx tiff_endianness)
      else
        for channel = 0 to samples_per_pixel - 1 do
          let byte_off = bytes_per_channel * channel in
          Genarray.set arr [| y; x; channel |]
            (read_value buf (buf_idx + byte_off) tiff_endianness)
        done

  let decompress ~sparse raw_buf expected_size compression =
    match (sparse, compression) with
    | true, _ | false, Ifd.No_compression -> raw_buf
    | false, LZW ->
        let out = Cstruct.create expected_size in
        Lzw.decode raw_buf out;
        out
    | false, DEFLATE | false, ADOBE_DEFLATE ->
        let out = Cstruct.create expected_size in
        Deflate.decode raw_buf out;
        out
    | _ -> failwith "Unsupported compression"

  let maybe_apply_predictor buf ~predictor ~bits_per_sample ~row_bytes
      ~samples_per_pixel ~num_rows =
    match predictor with
    | Ifd.HorizontalDifferencing ->
        let bytes_per_sample = List.hd bits_per_sample / 8 in
        apply_horizontal_differencing buf ~row_bytes ~bytes_per_sample
          ~samples_per_pixel ~num_rows
    | _ -> ()

  let read_stripped_data t ro plane window arr_type read_value =
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
    let compression = Ifd.compression ifd in
    let predictor = Ifd.predictor ifd in
    let strip_offsets_length = Array.length strip_offsets in
    if strip_offsets_length = Array.length strip_bytecounts then (
      let arr =
        if samples_per_pixel = 1 || planar_configuration = Planar then
          Genarray.create arr_type c_layout [| window.ysize; window.xsize |]
        else
          Genarray.create arr_type c_layout
            [| window.ysize; window.xsize; samples_per_pixel |]
      in

      let set_pixel =
        make_set_pixel ~samples_per_pixel ~planar_configuration ~bytes_per_pixel
          ~bits_per_sample ~tiff_endianness read_value
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
          match (sparse, compression) with
          | true, _ | false, No_compression ->
              if Cstruct.length raw_strip_buffer < expected_size then
                failwith "Strip is unexpectedly short";
              raw_strip_buffer
          | _ -> decompress ~sparse raw_strip_buffer expected_size compression
        in

        maybe_apply_predictor strip_buffer ~predictor ~bits_per_sample
          ~row_bytes:(width * bytes_per_pixel) ~samples_per_pixel
          ~num_rows:rows_in_strip;

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
              set_pixel arr strip_buffer (index * bytes_per_pixel)
                (y_offset - window.yoff) (!x_offset - window.xoff);
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

  let read_tiled_data t ro plane window arr_type read_value =
    let ifd = t.ifd in
    let samples_per_pixel = Ifd.samples_per_pixel ifd in
    let img_width = Ifd.width ifd in
    let img_height = Ifd.height ifd in
    let bits_per_sample = Ifd.bits_per_sample ifd in
    let planar_configuration = Ifd.planar_configuration ifd in
    let bytes_per_pixel =
      match planar_configuration with
      | Chunky -> List.fold_left Int.add 0 bits_per_sample / 8
      | Planar -> List.nth bits_per_sample plane / 8
      | Unknown _ ->
          failwith "Should not get this far if planar config isn't recognised."
    in
    let tile_w = Ifd.tile_width ifd in
    let tile_h = Ifd.tile_height ifd in
    let tiles_across = Ifd.tiles_across ifd in
    let tiles_down = Ifd.tiles_down ifd in
    let tile_offsets = Array.of_list (Ifd.data_offsets ifd) in
    let tile_bytecounts = Array.of_list (Ifd.data_bytecounts ifd) in
    let tiff_endianness = t.header.byte_order in
    let compression = Ifd.compression ifd in
    let predictor = Ifd.predictor ifd in

    (* For planar TIFFs, each plane has its own set of tiles *)
    let tiles_per_plane = tiles_across * tiles_down in
    let tile_base = plane * tiles_per_plane in

    let arr =
      if samples_per_pixel = 1 || planar_configuration = Planar then
        Genarray.create arr_type c_layout [| window.ysize; window.xsize |]
      else
        Genarray.create arr_type c_layout
          [| window.ysize; window.xsize; samples_per_pixel |]
    in

    let set_pixel =
      make_set_pixel ~samples_per_pixel ~planar_configuration ~bytes_per_pixel
        ~bits_per_sample ~tiff_endianness read_value
    in

    (* Which tiles overlap the window? *)
    let first_tile_col = window.xoff / tile_w in
    let last_tile_col = (window.xoff + window.xsize - 1) / tile_w in
    let first_tile_row = window.yoff / tile_h in
    let last_tile_row = (window.yoff + window.ysize - 1) / tile_h in

    let expected_size = tile_h * tile_w * bytes_per_pixel in

    let decode_tile tile_idx =
      let raw_tile_length = tile_bytecounts.(tile_idx) in
      let raw_tile_offset = tile_offsets.(tile_idx) in
      let sparse = raw_tile_length = 0 && raw_tile_offset = 0 in
      let tile_length = if sparse then expected_size else raw_tile_length in
      let raw_buf = Cstruct.create tile_length in
      if not sparse then
        ro ~file_offset:(Optint.Int63.of_int raw_tile_offset) [ raw_buf ];
      let buf = decompress ~sparse raw_buf expected_size compression in
      maybe_apply_predictor buf ~predictor ~bits_per_sample
        ~row_bytes:(tile_w * bytes_per_pixel) ~samples_per_pixel
        ~num_rows:tile_h;
      buf
    in

    for tile_row = first_tile_row to last_tile_row do
      for tile_col = first_tile_col to last_tile_col do
        let tile_idx = tile_base + (tile_row * tiles_across) + tile_col in
        let buf = decode_tile tile_idx in
        let tile_origin_x = tile_col * tile_w in
        let tile_origin_y = tile_row * tile_h in
        (* Clamp to actual image dimensions for edge tiles *)
        let eff_h = min tile_h (img_height - tile_origin_y) in
        let eff_w = min tile_w (img_width - tile_origin_x) in
        for row = 0 to eff_h - 1 do
          let y = tile_origin_y + row in
          if y >= window.yoff && y < window.yoff + window.ysize then
            for col = 0 to eff_w - 1 do
              let x = tile_origin_x + col in
              if x >= window.xoff && x < window.xoff + window.xsize then
                set_pixel arr buf
                  (((row * tile_w) + col) * bytes_per_pixel)
                  (y - window.yoff) (x - window.xoff)
            done
        done
      done
    done;
    arr

  let write_data plane window tiff data w write_value =
    let ifd = ifd tiff in
    let height = Ifd.height ifd in
    let width = Ifd.width ifd in
    let samples_per_pixel = Ifd.samples_per_pixel ifd in
    let bits_per_sample = Ifd.bits_per_sample ifd in
    let strip_offsets = Ifd.data_offsets ifd in
    let strip_bytecounts = Ifd.data_bytecounts ifd in
    let planar_configuration = Ifd.planar_configuration ifd in
    let bytes_per_pixel =
      match planar_configuration with
      | Chunky -> List.fold_left Int.add 0 bits_per_sample / 8
      | Planar -> List.nth bits_per_sample plane / 8
      | Unknown _ ->
          failwith "Should not get this far if planar config isn't recognised."
    in
    let bytes_per_channel = bytes_per_pixel / List.length bits_per_sample in
    let rows_per_strip = Ifd.rows_per_strip ifd in
    let endian = tiff.header.byte_order in
    let strips_per_plane =
      List.length strip_offsets / List.length bits_per_sample
    in

    let first_strip =
      (window.yoff / rows_per_strip) + (strips_per_plane * plane)
    in

    let row_index =
      ref ((first_strip - (strips_per_plane * plane)) * rows_per_strip)
    in

    let strip_offsets_length = List.length strip_offsets in
    if strip_offsets_length = List.length strip_bytecounts then
      (*Iterate through every strip *)
      List.iteri
        (fun strip_index strip ->
          let raw_strip_length = List.nth strip_bytecounts strip_index in
          let sparse = raw_strip_length = 0 && strip = 0 in
          (*Calculate the number of rows in the current strip*)
          let rows_in_strip =
            (*The last strip in the plane might have fewer rows*)
            if strip_index = strips_per_plane - 1 then
              height - (strip_index * rows_per_strip)
            else rows_per_strip
          in

          let expected_size = rows_in_strip * width * bytes_per_pixel in
          let strip_length =
            match sparse with
            | false -> raw_strip_length
            | true -> expected_size
          in
          let raw_strip_buffer = Cstruct.create strip_length in
          let strip_offset = Optint.Int63.of_int strip in

          for inner_row = 0 to rows_in_strip - 1 do
            let y_offset = inner_row + !row_index in
            if y_offset >= window.yoff + window.ysize || y_offset < window.yoff
            then ()
            else
              let x_offset = ref window.xoff in

              while !x_offset <= window.xoff + window.xsize - 1 do
                let index = !x_offset + (inner_row * width) in
                if samples_per_pixel = 1 then
                  let value =
                    Genarray.get data
                      [| y_offset - window.yoff; !x_offset - window.xoff |]
                  in

                  write_value raw_strip_buffer
                    (index * samples_per_pixel)
                    value endian
                else
                  for channel = 0 to samples_per_pixel - 1 do
                    let byte_off = bytes_per_channel * channel in
                    let value =
                      Genarray.get data
                        [|
                          y_offset - window.yoff;
                          !x_offset - window.xoff;
                          channel;
                        |]
                    in
                    write_value raw_strip_buffer
                      ((index * bytes_per_pixel) + byte_off)
                      value endian
                  done;

                x_offset := !x_offset + 1
              done
          done;
          row_index := !row_index + rows_in_strip;

          w ~file_offset:strip_offset [ raw_strip_buffer ])
        strip_offsets
    else
      raise
        (Invalid_argument
           "strip_offsets and strip_bytecounts are of different lengths")
end

let pp_window fmt window =
  Fmt.pf fmt "{ xoff=%i, yoff=%i, xsize=%i, ysize=%i }" window.xoff window.yoff
    window.xsize window.ysize

(* have to specify all 4 or else it defaults to whole file*)
let data (type repr kind) ?plane ?window (t : (repr, kind) t) (f : File.ro) :
    (repr, kind) Data.t =
  let ifd = ifd t in
  (* Check the window, if any, makes sense for the TIFF file *)
  let () =
    match window with
    | None -> ()
    | Some window ->
        let w = Ifd.width ifd and h = Ifd.height ifd in
        let win_w = window.xoff + window.xsize in
        let win_h = window.yoff + window.ysize in
        if Int.compare win_w w > 0 || Int.compare win_h h > 0 then
          Fmt.invalid_arg "Window %a for data (%i, %i)" pp_window window w h
  in
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
  let read =
    if Ifd.is_tiled ifd then Data.read_tiled_data else Data.read_stripped_data
  in
  match (t.data_type, sample_format, bpp) with
  | Uint8, UnsignedInteger, 8 ->
      read t f plane window Bigarray.int8_unsigned Data.read_uint8_value
  | Int8, SignedInteger, 8 ->
      read t f plane window Bigarray.int8_signed Data.read_int8_value
  | Uint16, UnsignedInteger, 16 ->
      read t f plane window Bigarray.int16_unsigned Data.read_uint16_value
  | Int16, SignedInteger, 16 ->
      read t f plane window Bigarray.int16_signed Data.read_int16_value
  | Uint32, UnsignedInteger, 32 ->
      Fmt.invalid_arg "Unsigned 32-bit coming soon..."
  | Int32, SignedInteger, 32 ->
      read t f plane window Bigarray.int32 Data.read_int32_value
  | Float32, IEEEFloatingPoint, 32 ->
      read t f plane window Bigarray.float32 Data.read_float32_value
  | Float64, IEEEFloatingPoint, 64 ->
      read t f plane window Bigarray.float64 Data.read_float64_value
  | typ, fmt, bpp ->
      Fmt.invalid_arg "datatype not correct for plane: %a, %a, %i bpp" pp_kind
        typ Ifd.pp_sample_format fmt bpp

let add_data (type repr kind) ?(plane = None) ?(window = None)
    (tiff : (repr, kind) t) (data : (repr, kind) Data.t) (w : File.wo) : _ =
  let ifd = ifd tiff in

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
  let window = Data.get_window ifd window in
  let plane = Data.get_plane ifd plane in
  let sample_format = Ifd.sample_format ifd in
  let bpp = List.nth (Ifd.bits_per_sample ifd) plane in

  match (tiff.data_type, sample_format, bpp) with
  | Uint8, UnsignedInteger, 8 ->
      Data.write_data plane window tiff data w Data.write_uint8_value
  | Int8, SignedInteger, 8 ->
      Data.write_data plane window tiff data w Data.write_int8_value
  | Uint16, UnsignedInteger, 16 ->
      Data.write_data plane window tiff data w Data.write_uint16_value
  | Int16, SignedInteger, 16 ->
      Data.write_data plane window tiff data w Data.write_int16_value
  | Uint32, UnsignedInteger, 32 ->
      Data.write_data plane window tiff data w Data.write_uint32_value
  | Int32, SignedInteger, 32 ->
      Data.write_data plane window tiff data w Data.write_uint32_value
  | Float32, IEEEFloatingPoint, 32 ->
      Data.write_data plane window tiff data w Data.write_float32_value
  | Float64, IEEEFloatingPoint, 64 ->
      Data.write_data plane window tiff data w Data.write_float64_value
  | typ, fmt, bpp ->
      Fmt.invalid_arg "datatype not correct for plane %a %a %i bpp" pp_kind typ
        Ifd.pp_sample_format fmt bpp

let to_file (type repr kind) ?(plane = None) ?(window = None)
    (tiff : (repr, kind) t) (data : (repr, kind) Data.t) (w : File.wo) =
  Ifd.write_header w tiff.header;
  Ifd.write_ifd ~file_offset:tiff.header.offset tiff.header w tiff.ifd;
  add_data ~plane ~window tiff data w

module Private = struct
  module Lzw = Lzw
  module Deflate = Deflate
end
