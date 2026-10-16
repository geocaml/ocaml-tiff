let () = Alcobar.run "tiff" [ Fuzz_lzw.suite; Fuzz_endian.suite ]
