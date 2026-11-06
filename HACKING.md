# ocaml-tiff

The ocaml-tiff library provides a pure OCaml implementation for reading and
decoding TIFF (Tagged Image File Format) images. It is designed to parse TIFF
structures, extract metadata, and reconstruct pixel data.


[TIFF file format](https://en.wikipedia.org/wiki/TIFF)


## TIFF decoding process

The decoding process begins with reading the TIFF file into a memory buffer,
represented as `Cstruct.t`, which holds raw bytes. This buffer is then passed
to the main decoding logic in `tiff.ml`, which coordinates the entire decoding
workflow. The process begins by calling `ifd.ml` to parse the TIFF header,
determine the byte order (handled by `endian.ml`), and detect the file type
(Classic TIFF or BigTIFF). The decoder then navigates through the Image File
Directories (IFDs) to extract metadata, compression details, and offsets to
image data. Depending on the compression scheme (LZW or Deflate), the library
dispatches decompression to the appropriate module (e.g. `lzw.ml`). 

## Current state of the codebase

The library is still experimental: the API may change, and even the features
listed as supported below may have bugs or missing edge cases.

### Currently supported
* Reading TIFF files
* Writing TIFF files (uncompressed only)
* Classic TIFFs (32-bit offsets)
* BigTIFF (64-bit offsets)
* Strip-based storage
* Uncompressed (No compression)
* LZW Compression
* Horizontal differencing predictor
* Basic GeoTIFF metadata, including the GeoKeyDirectory

### Future support
* Tile-based storage
* Deflate Compression (ZIP)
* JPEG Compression
