open Eio

let () =
  Eio_main.run @@ fun env ->
  let fs = Stdenv.fs env in
  Path.(with_open_in (fs / "test/test-ignore/frog_uncompressed.tiff")) @@ fun r ->
  let tiff = Tiff.from_file (File.pread_exact r) in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  let height = Tiff.Ifd.height ifd in 
  let width = Tiff.Ifd.width ifd in
  let rows_per_strip = Tiff.Ifd.rows_per_strip ifd in
  Eio.traceln "Entries:\n %a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "File size: %ix%i" height width;
  Eio.traceln "Samples per pixel: %i" (Tiff.Ifd.samples_per_pixel ifd);
  Eio.traceln "Bits per sample: %a"
    Fmt.(list int)
    (Tiff.Ifd.bits_per_sample ifd);
  let data_offsets = Tiff.Ifd.data_offsets ifd in
  let data_bytecounts = Tiff.Ifd.data_bytecounts ifd in
  Eio.traceln "Offsets: %a"
    Fmt.(list ~sep:(any ", ") int)
    (data_offsets);
  Eio.traceln "Counts: %a"
    Fmt.(list ~sep:(any ", ") int)
    (data_bytecounts);
  (* let total = Tiff.read_data_float32 (File.pread_exact r) data_offsets data_bytecounts in *)
  
  let arr_test = Tiff.read_data_float32 (File.pread_exact r) data_offsets data_bytecounts rows_per_strip width in

  let sum_arr_test = Tiff.sum_array arr_test in 

  (* Eio.traceln "Total from o.g. method: %.12f" total; *)
  Eio.traceln "New total: %.12f" sum_arr_test; 
  Eio.traceln "File opened successfully.";;

