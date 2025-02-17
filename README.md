# ocaml-tiff

*Status: WIP & Experimental*

Supports both TIFF and BigTIFF files. The underlying IO mechanisms are expected to be provided by the user using a library of their choice. For example, you could use [Eio](https://github.com/ocaml-multicore/eio).

```ocaml
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(with_open_in (fs / "test/cea.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  let data = Tiff.data tiff ro Tiff.Data.Uint8 in 
  let sum = Owl_base_dense_ndarray_generic.sum' data in

  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "%ix%i"
    (Tiff.Ifd.height (Tiff.ifd tiff))
    (Tiff.Ifd.width (Tiff.ifd tiff));
  Eio.traceln "offsets: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_offsets ifd);
  Eio.traceln "counts: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_bytecounts ifd);
  Eio.traceln "sum: %i" sum
+tag: image-width, field: short, count: 1, value/offset: 514
+tag: image-length, field: short, count: 1, value/offset: 515
+tag: bits-per-sample, field: short, count: 1, value/offset: 8
+tag: compression, field: short, count: 1, value/offset: 1
+tag: photometric-interpretation, field: short, count: 1, value/offset: 1
+tag: strip-offsets, field: long, count: 35, value/offset: 270474
+tag: samples-per-pixel, field: short, count: 1, value/offset: 1
+tag: rows-per-strip, field: short, count: 1, value/offset: 15
+tag: strip-byte-counts, field: long, count: 35, value/offset: 270614
+tag: planar-configuration, field: short, count: 1, value/offset: 1
+tag: sample-format, field: short, count: 1, value/offset: 1
+tag: model-pixel-scale, field: double, count: 3, value/offset: 270754
+tag: model-tiepoint, field: double, count: 6, value/offset: 270778
+tag: geo-key-directory, field: short, count: 60, value/offset: 270826
+tag: geo-double-params, field: double, count: 4, value/offset: 270946
+tag: geo-ascii-params, field: ascii, count: 15, value/offset: 270978
+515x514
+offsets: 426, 8136, 15846, 23556, 31266, 38976, 46686, 54396, 62106, 69816, 77526, 85236, 92946, 100656, 108366, 116076, 123786, 131496, 139206, 146916, 154626, 162336, 170046, 177756, 185466, 193176, 200886, 208596, 216306, 224016, 231726, 239436, 247146, 254856, 262566
+counts: 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710
+sum: 27304701
- : unit = ()
```

Windows can be used to narrow the area returned by `data`.

```ocaml
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(with_open_in (fs / "test/uniform.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in 
  let tiff = Tiff.from_file ro in
  let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  let data = Tiff.data ~window tiff ro Tiff.Data.Uint8 in
  let res = Owl_base_dense_ndarray_generic.sum' data in
  Eio.traceln "We expect %i and got %i" (10 * 10 * 128) res;;
+We expect 12800 and got 12800
- : unit = ()
```
