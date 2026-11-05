(* JPEG marker and segment reading. *)

let malformed message =
  Error.raise_error Error.Malformed_file ("JPEG: " ^ message)

let unsupported message =
  Error.raise_error Error.Unsupported_feature ("JPEG: " ^ message)

type reader = { data : Cstruct.t; mutable pos : int }

let remaining reader = Cstruct.length reader.data - reader.pos

let require reader count =
  if count < 0 || count > remaining reader then malformed "Truncated input"

let read_u8 reader =
  require reader 1;
  let value = Cstruct.get_uint8 reader.data reader.pos in
  reader.pos <- reader.pos + 1;
  value

let read_u16 reader =
  require reader 2;
  let value = Cstruct.BE.get_uint16 reader.data reader.pos in
  reader.pos <- reader.pos + 2;
  value

let read_bytes reader count =
  require reader count;
  let value = Cstruct.sub reader.data reader.pos count in
  reader.pos <- reader.pos + count;
  value

let expect_soi reader =
  if read_u16 reader <> 0xffd8 then malformed "Missing start-of-image marker"

let read_marker reader =
  if read_u8 reader <> 0xff then malformed "Expected a marker";
  let rec skip_fill () =
    match read_u8 reader with
    | 0xff -> skip_fill ()
    | 0x00 -> malformed "Unexpected stuffed byte outside scan data"
    | marker -> marker
  in
  skip_fill ()

let read_segment reader =
  let length = read_u16 reader in
  if length < 2 then malformed "Invalid segment length";
  let data = read_bytes reader (length - 2) in
  { data; pos = 0 }

let expect_segment_end reader =
  if remaining reader <> 0 then
    malformed "Unexpected bytes at the end of a segment"

type huffman = { counts : int array; symbols : int array }

type tables = {
  quantization : int array option array;
  dc : huffman option array;
  ac : huffman option array;
  global_quantization : bool array;
  global_dc : bool array;
  global_ac : bool array;
}

let create_tables () =
  {
    quantization = Array.make 4 None;
    dc = Array.make 4 None;
    ac = Array.make 4 None;
    global_quantization = Array.make 4 false;
    global_dc = Array.make 4 false;
    global_ac = Array.make 4 false;
  }

let read_quantization tables segment =
  while remaining segment > 0 do
    let info = read_u8 segment in
    let precision = info lsr 4 in
    let id = info land 0x0f in
    if id > 3 then malformed "Invalid quantization table identifier";
    if tables.global_quantization.(id) then
      malformed "Image segment redefines a shared quantization table";
    if precision > 1 then malformed "Invalid quantization table precision";
    if precision = 1 then unsupported "16-bit quantization tables";
    let values =
      Array.init 64 (fun _ ->
          let value = read_u8 segment in
          if value = 0 then malformed "Zero quantization value";
          value)
    in
    tables.quantization.(id) <- Some values
  done

let read_huffman tables segment =
  while remaining segment > 0 do
    let info = read_u8 segment in
    let table_class = info lsr 4 in
    let id = info land 0x0f in
    if table_class > 1 || id > 3 then
      malformed "Invalid Huffman table identifier";
    let is_global =
      if table_class = 0 then tables.global_dc.(id) else tables.global_ac.(id)
    in
    if is_global then malformed "Image segment redefines a shared Huffman table";
    let counts = Array.init 16 (fun _ -> read_u8 segment) in
    let total = Array.fold_left ( + ) 0 counts in
    if total = 0 || total > 256 then malformed "Invalid Huffman symbol count";
    let available = ref 1 in
    Array.iter
      (fun count ->
        available := (!available * 2) - count;
        if !available < 0 then malformed "Oversubscribed Huffman table")
      counts;
    if !available = 0 then
      malformed "Huffman table uses the reserved all-ones code";
    let symbols = Array.init total (fun _ -> read_u8 segment) in
    let table = { counts; symbols } in
    if table_class = 0 then tables.dc.(id) <- Some table
    else tables.ac.(id) <- Some table
  done

type bit_reader = {
  source : reader;
  mutable current : int;
  mutable bits_left : int;
}

let create_bit_reader source = { source; current = 0; bits_left = 0 }

let read_scan_byte source =
  let value = read_u8 source in
  if value <> 0xff then value
  else
    match read_u8 source with
    | 0x00 -> 0xff
    | _ -> malformed "Unexpected marker inside compressed scan data"

let read_bit bits =
  if bits.bits_left = 0 then (
    bits.current <- read_scan_byte bits.source;
    bits.bits_left <- 8);
  bits.bits_left <- bits.bits_left - 1;
  (bits.current lsr bits.bits_left) land 1

let read_bits bits count =
  if count < 0 || count > 16 then malformed "Invalid bit count";
  let value = ref 0 in
  for _ = 1 to count do
    value := (!value lsl 1) lor read_bit bits
  done;
  !value

let decode_huffman bits table =
  let rec search length code first symbol_offset =
    if length > 16 then malformed "Invalid Huffman code";
    let code = (code lsl 1) lor read_bit bits in
    let count = table.counts.(length - 1) in
    let index = code - first in
    if index >= 0 && index < count then table.symbols.(symbol_offset + index)
    else search (length + 1) code ((first + count) lsl 1) (symbol_offset + count)
  in
  search 1 0 0 0

let receive_extend bits count =
  if count = 0 then 0
  else (
    if count < 0 || count > 16 then malformed "Invalid coefficient bit count";
    let value = read_bits bits count in
    if value < 1 lsl (count - 1) then value - ((1 lsl count) - 1) else value)

let zigzag =
  [|
    0;
    1;
    8;
    16;
    9;
    2;
    3;
    10;
    17;
    24;
    32;
    25;
    18;
    11;
    4;
    5;
    12;
    19;
    26;
    33;
    40;
    48;
    41;
    34;
    27;
    20;
    13;
    6;
    7;
    14;
    21;
    28;
    35;
    42;
    49;
    56;
    57;
    50;
    43;
    36;
    29;
    22;
    15;
    23;
    30;
    37;
    44;
    51;
    58;
    59;
    52;
    45;
    38;
    31;
    39;
    46;
    53;
    60;
    61;
    54;
    47;
    55;
    62;
    63;
  |]

let decode_block bits dc_table ac_table quantization previous_dc =
  let coefficients = Array.make 64 0.0 in
  let category = decode_huffman bits dc_table in
  if category > 11 then malformed "Invalid baseline DC coefficient category";
  let dc = previous_dc + receive_extend bits category in
  coefficients.(0) <- float_of_int dc *. float_of_int quantization.(0);
  let position = ref 1 in
  while !position < 64 do
    let symbol = decode_huffman bits ac_table in
    let run = symbol lsr 4 in
    let size = symbol land 0x0f in
    if size = 0 then
      if run = 0 then position := 64
      else if run = 15 then (
        if !position + 16 > 64 then malformed "AC zero run exceeds block";
        position := !position + 16)
      else malformed "Invalid AC zero-run symbol"
    else (
      if size > 10 then malformed "Invalid baseline AC coefficient category";
      position := !position + run;
      if !position >= 64 then malformed "AC coefficient exceeds block";
      let value = receive_extend bits size in
      coefficients.(zigzag.(!position)) <-
        float_of_int value *. float_of_int quantization.(!position);
      incr position)
  done;
  (dc, coefficients)

let idct_basis =
  Array.init 8 (fun position ->
      Array.init 8 (fun frequency ->
          let scale = if frequency = 0 then 1.0 /. sqrt 2.0 else 1.0 in
          scale
          *. cos
               (float_of_int (((2 * position) + 1) * frequency)
               *. Float.pi /. 16.0)))

let inverse_dct coefficients =
  let intermediate = Array.make 64 0.0 in
  let pixels = Array.make 64 0 in
  for v = 0 to 7 do
    for x = 0 to 7 do
      let sum = ref 0.0 in
      for u = 0 to 7 do
        sum := !sum +. (coefficients.((v * 8) + u) *. idct_basis.(x).(u))
      done;
      intermediate.((v * 8) + x) <- !sum
    done
  done;
  for y = 0 to 7 do
    for x = 0 to 7 do
      let sum = ref 0.0 in
      for v = 0 to 7 do
        sum := !sum +. (intermediate.((v * 8) + x) *. idct_basis.(y).(v))
      done;
      let value = int_of_float (floor ((!sum /. 4.0) +. 128.5)) in
      pixels.((y * 8) + x) <- max 0 (min 255 value)
    done
  done;
  pixels

type frame = {
  width : int;
  height : int;
  component_id : int;
  quantization_id : int;
}

let read_frame segment =
  let precision = read_u8 segment in
  if precision <> 8 then unsupported "Only 8-bit JPEG samples are supported";
  let height = read_u16 segment in
  let width = read_u16 segment in
  if width = 0 || height = 0 then malformed "Invalid image dimensions";
  let components = read_u8 segment in
  if components <> 1 then
    unsupported "Only single-component JPEG is currently supported";
  let component_id = read_u8 segment in
  let sampling = read_u8 segment in
  if sampling <> 0x11 then
    unsupported "Only 1x1 sampling is currently supported";
  let quantization_id = read_u8 segment in
  if quantization_id > 3 then
    malformed "Invalid frame quantization table identifier";
  expect_segment_end segment;
  { width; height; component_id; quantization_id }

let read_scan_header frame segment =
  let components = read_u8 segment in
  if components <> 1 then
    unsupported "Only single-component scans are currently supported";
  let component_id = read_u8 segment in
  if component_id <> frame.component_id then
    malformed "Scan component does not match frame";
  let selectors = read_u8 segment in
  let dc_id = selectors lsr 4 in
  let ac_id = selectors land 0x0f in
  if dc_id > 3 || ac_id > 3 then
    malformed "Invalid scan Huffman table identifier";
  let spectral_start = read_u8 segment in
  let spectral_end = read_u8 segment in
  let approximation = read_u8 segment in
  if spectral_start <> 0 || spectral_end <> 63 || approximation <> 0 then
    unsupported "Only baseline sequential scans are supported";
  expect_segment_end segment;
  (dc_id, ac_id)

let require_table tables id description =
  match tables.(id) with
  | Some table -> table
  | None -> malformed ("Missing " ^ description ^ " table")

let align_scan bits =
  let mask = (1 lsl bits.bits_left) - 1 in
  if bits.current land mask <> mask then malformed "Invalid scan padding bits";
  bits.bits_left <- 0

let decode_scan source tables frame dc_id ac_id restart_interval output =
  let quantization =
    require_table tables.quantization frame.quantization_id "quantization"
  in
  let dc_table = require_table tables.dc dc_id "DC Huffman" in
  let ac_table = require_table tables.ac ac_id "AC Huffman" in
  if frame.width > max_int / frame.height then
    malformed "Image dimensions exceed supported size";
  if Cstruct.length output <> frame.width * frame.height then
    malformed "JPEG dimensions do not match the output buffer";
  let bits = create_bit_reader source in
  let blocks_across = (frame.width + 7) / 8 in
  let blocks_down = (frame.height + 7) / 8 in
  let previous_dc = ref 0 in
  let block_number = ref 0 in
  let next_restart = ref 0 in
  for block_y = 0 to blocks_down - 1 do
    for block_x = 0 to blocks_across - 1 do
      if
        restart_interval > 0 && !block_number > 0
        && !block_number mod restart_interval = 0
      then (
        align_scan bits;
        let marker = read_marker source in
        if marker <> 0xd0 + !next_restart then
          malformed "Unexpected restart marker";
        next_restart := (!next_restart + 1) mod 8;
        previous_dc := 0);
      let dc, coefficients =
        decode_block bits dc_table ac_table quantization !previous_dc
      in
      previous_dc := dc;
      let pixels = inverse_dct coefficients in
      for y = 0 to 7 do
        let image_y = (block_y * 8) + y in
        if image_y < frame.height then
          for x = 0 to 7 do
            let image_x = (block_x * 8) + x in
            if image_x < frame.width then
              Cstruct.set_uint8 output
                ((image_y * frame.width) + image_x)
                pixels.((y * 8) + x)
          done
      done;
      incr block_number
    done
  done;
  align_scan bits

let load_shared_tables tables data =
  let source = { data; pos = 0 } in
  expect_soi source;
  let rec loop () =
    match read_marker source with
    | 0xd9 ->
        if remaining source <> 0 then
          malformed "Unexpected bytes after shared tables"
    | 0xdb ->
        read_quantization tables (read_segment source);
        loop ()
    | 0xc4 ->
        read_huffman tables (read_segment source);
        loop ()
    | 0xdd ->
        let segment = read_segment source in
        ignore (read_u16 segment);
        expect_segment_end segment;
        loop ()
    | 0xcc ->
        ignore (read_segment source);
        loop ()
    | marker when marker >= 0xe0 && marker <= 0xef ->
        ignore (read_segment source);
        loop ()
    | 0xfe ->
        ignore (read_segment source);
        loop ()
    | _ -> malformed "Unexpected marker in shared tables"
  in
  loop ()

let decode ?tables:shared_tables ~width ~height input output =
  let tables = create_tables () in
  (match shared_tables with
  | None -> ()
  | Some data -> load_shared_tables tables data);
  Array.iteri
    (fun id value -> tables.global_quantization.(id) <- Option.is_some value)
    tables.quantization;
  Array.iteri
    (fun id value -> tables.global_dc.(id) <- Option.is_some value)
    tables.dc;
  Array.iteri
    (fun id value -> tables.global_ac.(id) <- Option.is_some value)
    tables.ac;
  let source = { data = input; pos = 0 } in
  expect_soi source;
  let frame = ref None in
  let restart_interval = ref 0 in
  let rec loop () =
    match read_marker source with
    | 0xdb ->
        read_quantization tables (read_segment source);
        loop ()
    | 0xc4 ->
        read_huffman tables (read_segment source);
        loop ()
    | 0xc0 ->
        (match !frame with
        | Some _ -> malformed "Duplicate frame header"
        | None ->
            let value = read_frame (read_segment source) in
            if value.width <> width || value.height <> height then
              malformed "JPEG dimensions do not match TIFF dimensions";
            frame := Some value);
        loop ()
    | 0xdd ->
        let segment = read_segment source in
        restart_interval := read_u16 segment;
        expect_segment_end segment;
        loop ()
    | 0xda ->
        let frame =
          match !frame with
          | Some value -> value
          | None -> malformed "Scan appears before frame header"
        in
        let dc_id, ac_id = read_scan_header frame (read_segment source) in
        decode_scan source tables frame dc_id ac_id !restart_interval output;
        finish ()
    | 0xd9 -> malformed "Image ends before scan data"
    | marker when marker >= 0xe0 && marker <= 0xef ->
        ignore (read_segment source);
        loop ()
    | 0xfe ->
        ignore (read_segment source);
        loop ()
    | _ -> unsupported "Unsupported JPEG marker or coding process"
  and finish () =
    match read_marker source with
    | 0xd9 ->
        if remaining source <> 0 then
          malformed "Unexpected bytes after end-of-image marker"
    | 0xfe ->
        ignore (read_segment source);
        finish ()
    | marker when marker >= 0xe0 && marker <= 0xef ->
        ignore (read_segment source);
        finish ()
    | _ -> unsupported "Additional scans or trailing JPEG markers"
  in
  loop ()
