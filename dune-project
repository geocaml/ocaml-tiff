(lang dune 3.9)
(using mdx 0.3)

(name tiff)

(generate_opam_files true)

(package
 (name tiff)
 (synopsis "Pure OCaml TIFF reader and writer")
 (description "This library provides low-level access to TIFF files for reading and writing with a small dependency cone.")
 (depends
   fmt
   cstruct
   optint
   (ocamlformat (and (= "0.27.0") :with-dev-setup)) 
   (bechamel :with-test)
   (bechamel-notty :with-test)
   (mdx :with-test)
   (ounit2 :with-test)
   (eio_main :with-test)
   (owl-base :with-test)))
