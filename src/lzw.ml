let rec get_bits bytedata offset count =
  match count with
  | 0 -> 0
  | n ->
	  let byte = int_of_char (Cstruct.get_char bytedata (offset / 8)) in
	  let bitoffset = (offset mod 8) in
	  let bit = (byte lsr (7 - bitoffset)) land 1 in
	  (bit lsl (n - 1)) + ((get_bits bytedata (offset + 1) (n - 1)))

let flatten_codes ?pad bits_per_pixel code_list =
  let pad = match pad with None -> false | Some x -> x in
  let code_list =
	match pad with
	| false -> code_list
	| true ->
		let total_bits =
		  List.fold_left (fun acc (_, bit_count) -> acc + bit_count) 0 code_list
		in
		let extra = bits_per_pixel - (total_bits mod bits_per_pixel) in
		List.rev ((Z.zero, extra) :: List.rev code_list)
  in
  let total_bits =
	List.fold_left (fun acc (_, bit_count) -> acc + bit_count) 0 code_list
  in
  if total_bits mod bits_per_pixel != 0 then
	failwith
	  (Printf.sprintf "unaligned result: %d bits, %d bits per pixel" total_bits
		 bits_per_pixel);
  let result = Cstruct.create (total_bits / bits_per_pixel) in
  let _, (_, rem) =
	List.fold_left
	  (fun (bit_offset, (rem_bits, rem_len)) (data, bit_count) ->
		let merged = (Z.((data lsl rem_len) + rem_bits), bit_count + rem_len) in

		let rec inner result_bit_offset (mbits, mlen) =
		  let byte_offset = result_bit_offset / bits_per_pixel in

		  if mlen < bits_per_pixel then (result_bit_offset, (mbits, mlen))
		  else (
			Cstruct.set_char result byte_offset
			  (char_of_int
				 (Z.to_int (Z.logand mbits Z.((one lsl bits_per_pixel) - one))));
			inner
			  (result_bit_offset + bits_per_pixel)
			  (Z.shift_right mbits bits_per_pixel, mlen - bits_per_pixel))
		in
		inner bit_offset merged)
	  (0, (Z.zero, 0))
	  code_list
  in
  if rem != 0 then failwith (Printf.sprintf "%d bits remaining at end" rem);
  result

let add_codes a b =
	let a_data, a_bitcount = a and b_data, b_bitcount = b in
	(Z.(a_data lor (b_data lsl a_bitcount)), a_bitcount + b_bitcount)

let build_table_entry a b character_size =
	add_codes a (Z.(fst b land ((one lsl character_size) - one)), character_size)

let decode input =
	let initial_code_size = 8 in
    let clear_code = 1 lsl initial_code_size in
	let end_code = clear_code + 1 in
	let dict = Array.make 4096 (Z.zero, 0) in

	let rec inner in_offset code_size next_code_index prev_code =
	  let code = get_bits input in_offset code_size in
	  if code == clear_code then
		let new_code_size = initial_code_size + 1 in
		let next_code = get_bits input (in_offset + code_size) new_code_size in
		(Z.of_int next_code, initial_code_size)
		:: inner
			 (in_offset + code_size + new_code_size)
			 new_code_size (clear_code + 2)
			 (Z.of_int next_code, initial_code_size)
	  else if code == end_code then []
	  else
		let entry =
		  if code < clear_code then (Z.of_int code, initial_code_size)
		  else if code < next_code_index then dict.(code)
		  else build_table_entry prev_code prev_code initial_code_size
		in

		if next_code_index < 4096 then
		  dict.(next_code_index) <-
			build_table_entry prev_code entry initial_code_size;
		let prev_entry, new_code_size, new_code_index =
		  match next_code_index with
		  | 4095 -> (prev_code, code_size, next_code_index)
		  | _ ->
			  let i = next_code_index + 1 in
			  ( entry,
				(if i >= ((1 lsl code_size) - 1) then code_size + 1 else code_size),
				i )
		in

		entry
		:: inner (in_offset + code_size) new_code_size new_code_index prev_entry

	in

	let c = inner 0 (initial_code_size + 1) (clear_code + 2) (Z.zero, 0) in

	flatten_codes initial_code_size c
