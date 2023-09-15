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
   optint))