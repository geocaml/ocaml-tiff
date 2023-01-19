# ocaml-tiff

*Status: WIP & Experimental*

Supports both TIFF and BigTIFF files.

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let open Eio in
  let r = Path.(open_in ~sw (env#fs / "test/cea.tiff")) in
  let tiff = Tiff.of_file (r :> File.ro) in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;
  Eio.traceln "%ix%i"
    (Tiff.Ifd.height (Tiff.ifd tiff))
    (Tiff.Ifd.width (Tiff.ifd tiff));
  Eio.traceln "offsets: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_offsets ifd);
  Eio.traceln "counts: %a"
    Fmt.(list ~sep:(any ", ") int)
    (Tiff.Ifd.data_bytecounts ifd)
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
+tag: unknown-33550, field: double, count: 3, value/offset: 270754
+tag: unknown-33922, field: double, count: 6, value/offset: 270778
+tag: unknown-34735, field: short, count: 60, value/offset: 270826
+tag: unknown-34736, field: double, count: 4, value/offset: 270946
+tag: unknown-34737, field: ascii, count: 15, value/offset: 270978
+515x514
+offsets: 426, 8136, 15846, 23556, 31266, 38976, 46686, 54396, 62106, 69816, 77526, 85236, 92946, 100656, 108366, 116076, 123786, 131496, 139206, 146916, 154626, 162336, 170046, 177756, 185466, 193176, 200886, 208596, 216306, 224016, 231726, 239436, 247146, 254856, 262566
+counts: 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710, 7710
- : unit = ()
```
