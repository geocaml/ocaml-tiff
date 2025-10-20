# ocaml-tiff
The ocaml-tiff library provides a pure OCaml implementation for reading and decoding TIFF (Tagged Image File Format) images. It is designed to parse TIFF structures, extract metadata, and reconstruct pixel data.


[TIFF file format](https://en.wikipedia.org/wiki/TIFF)


## Main file responsibilities in ocaml-tiff

### tiff.ml 
**Core TIFF decoding logic -** 
Parses TIFF headers, IFDs (Image File Directories), interprets TIFF tags, manages reading image metadata and pixel data, and handles dispatching decompression logic depending on tag values. 

### tiff.mli
**Public interface (API) -** Declares the types and exposed functions used for interaction with the TIFF decoding API.

### lzw.ml
**LZW decompression -** Implements the LZW decompression algorithm used for TIFF image strips. Called by tiff.ml when a TIFF uses LZW compression.

### lzw.mli
**Public interface for LZW -** Declares the LZW decompression function signature.

### unix
**Reads TIFF into memory -** Loads TIFF files from disk into Cstruct.t memory buffers, and passes them to the core TIFF logic. Acts as a small wrapper to connect filesystem access to the main decoding logic. 

### test_tiff.ml
**TIFF decoding tests -** End-to-end validation of TIFF file reading and decoding, checking both header interpretation and pixel data output.

### test_lzw.ml 
**LZW tests -** Tests the correctness of the LZW decompression algorithm with pre-compressed test data.

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