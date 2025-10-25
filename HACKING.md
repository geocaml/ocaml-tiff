# ocaml-tiff
The ocaml-tiff library provides a pure OCaml implementation for reading and decoding TIFF (Tagged Image File Format) images. It is designed to parse TIFF structures, extract metadata, and reconstruct pixel data.


[TIFF file format](https://en.wikipedia.org/wiki/TIFF)


## TIFF decoding process
The decoding process begins with reading the TIFF file into a memory buffer, represented as `Cstruct.t`, which holds raw bytes. This buffer is then passed to the main decoding logic in (`tiff.ml`), which begins by parsing the TIFF header to determine byte order (endianness) and file format (Classic TIFF or BigTIFF). The library then navigates through IFDs (Image File Directories), extracting metadata and locating pointers to image data. If compression is detected (like LZW), it may dispatch decompression tasks to the appropriate module (`lzw.ml`). The final output is pixel data structured in a 2D Bigarray, along with extracted metadata stored in memory.

## Current state of the codebase

### Currently supported
* Reading TIFF files
* Classic TIFFs (32-bit offsets)
* Strip-based storage
* Uncompressed (No compression)
* LZW Compression
* Basic GeoTIFF metadata

### Future support
* Writing TIFF files
* Tile-based storage
* Deflate Compression (ZIP)
* JPEG Compression
* Advanced GeoTIFF metadata (GeoKeyDirectory)