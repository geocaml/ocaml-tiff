# ocaml-tiff


A pure OCaml library for reading TIFF files. The underlying IO mechanisms are expected to be provided by the user using a library of their choice. For example, you could use [Eio](https://github.com/ocaml-multicore/eio).

```ocaml
module Arr = Owl_base_dense_ndarray_generic

let info ?window ?(kind=Tiff.Data.Uint8) f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(with_open_in (fs / f)) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tif = Tiff.from_file ro in
  let data = Tiff.data ?window tif ro kind in
  Eio.traceln "Shape: [%a]" Fmt.(array ~sep:Fmt.comma int) (Arr.shape data);
  Eio.traceln "Sum: %i" (Arr.sum' data)
```

We then apply our `info` function to two different TIFF files. The first only contains a single band of data.

```ocaml
# info "./testdata/cea.tiff";;
+Shape: [515, 514]
+Sum: 27304701
- : unit = ()
```

The second contains two bands of data (hence the extra dimension).

```ocaml
# info "./testdata/jello-gray.tiff";;
+Shape: [192, 256, 2]
+Sum: 16731734
- : unit = ()
```

Windows can be used to narrow the area returned by `data`.

```ocaml
# let window = Tiff.{ xoff = 0; yoff = 0; xsize = 10; ysize = 10 } in
  info ~window "./testdata/uniform.tiff";;
+Shape: [10, 10]
+Sum: 12800
- : unit = ()
```

Other files are a little more awkward, with different endianness and different strip sizes.

```ocaml
# Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.(with_open_in (fs / "testdata/jello-gray.tiff")) @@ fun r ->
  let ro = Eio.File.pread_exact r in
  let tiff = Tiff.from_file ro in
  let ifd = Tiff.ifd tiff in
  let entries = Tiff.Ifd.entries ifd in
  Eio.traceln "%a" Fmt.(list Tiff.Ifd.pp_entry) entries;;
+tag: image-width, field: short, count: 1, value/offset: 256
+tag: image-length, field: short, count: 1, value/offset: 192
+tag: bits-per-sample, field: short, count: 2, value/offset: 524296
+tag: compression, field: short, count: 1, value/offset: 1
+tag: photometric-interpretation, field: short, count: 1, value/offset: 1
+tag: unknown-266, field: short, count: 1, value/offset: 1
+tag: strip-offsets, field: long, count: 1, value/offset: 8
+tag: unknown-274, field: short, count: 1, value/offset: 1
+tag: samples-per-pixel, field: short, count: 1, value/offset: 2
+tag: rows-per-strip, field: short, count: 1, value/offset: 192
+tag: strip-byte-counts, field: long, count: 1, value/offset: 98304
+tag: planar-configuration, field: short, count: 1, value/offset: 1
+tag: unknown-297, field: short, count: 2, value/offset: 65536
+tag: unknown-338, field: short, count: 1, value/offset: 2
+tag: unknown-34675, field: undefined, count: 1992, value/offset: 98498
- : unit = ()
```
