# ocaml-tiff
The ocaml-tiff library provides a pure OCaml implementation for reading and decoding TIFF (Tagged Image File Format) images. It is designed to parse TIFF structures, extract metadata, and reconstruct pixel data.


[TIFF file format](https://en.wikipedia.org/wiki/TIFF)


## TIFF decoding process

The decoding process begins with reading the TIFF file into a memory buffer, represented as `Cstruct.t`, which holds raw bytes. This buffer is then passed to the main decoding logic in `tiff.ml`, which coordinates the entire decoding workflow. The process begins by calling `ifd.ml` to parse the TIFF header, determine the byte order (handled by `endian.ml`), and detect the file type (Classic TIFF or BigTIFF). The decoder then naigates through the Image File Directories (IFDs) to extract metadata, compression details, and offsets to image data. Depending on the compression scheme (LZW or Deflate), the library dispatches decompression to the appropriate module (e.g.,`lzw.ml`) . The decoded pixel data is organised into a 2D Bigarray, while metadata is stored in memory.

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
