let write_tiff filename data =
  let oc = open_out_bin filename in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      let dims = Bigarray.Genarray.dims data in
      let total_elements = Array.fold_left ( * ) 1 dims in
      let flat = Bigarray.reshape_1 data total_elements in
      for i = 0 to total_elements - 1 do
        output_byte oc (Bigarray.Array1.get flat i)
      done)

let read_created_tiff file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let cl = in_channel_length ic in
      let data = Bigarray.Array1.create Int8_unsigned Bigarray.c_layout cl in
      for i = 0 to cl - 1 do
        let byte = input_byte ic in
        Bigarray.Array1.set data i byte
      done;
      data)
  |> Bigarray.genarray_of_array1
