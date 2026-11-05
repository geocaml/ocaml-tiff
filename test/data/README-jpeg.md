# Reading JPEG-compressed TIFFs 🖼️

This document records the investigation, implementation and validation for [issue #19 — Read JPEG Compressed TIFFs](https://github.com/geocaml/ocaml-tiff/issues/19).

I started by reproducing the unsupported-compression error with a small grayscale image, then worked through the TIFF metadata, shared JPEG tables and compressed image data. The implementation now reads the supported grayscale JPEG strips through the existing TIFF API, without adding a runtime dependency.

## 🎯 Supported scope

The implementation supports:

- TIFF compression **7 (`JPEG`)**.
- **8-bit unsigned, single-component grayscale samples**.
- Baseline sequential JPEG using Huffman coding.
- **1 × 1 sampling**.
- Single-strip and multiple-strip images, including a shorter final strip.
- Image dimensions that are not multiples of eight.
- Shared quantization and Huffman tables supplied through `JPEGTables`.
- Complete JPEG streams containing their own tables.
- Restart intervals and restart-marker sequence validation.

The TIFF integration accepts `BlackIsZero` and `WhiteIsZero` photometric interpretations and returns decoded sample values without introducing colour conversion.

This contribution does not add support for:

- Colour or multi-component JPEG.
- Progressive, lossless or arithmetic-coded JPEG.
- Old-style JPEG compression (`OJPEG`, compression 6).
- 16-bit quantization tables.
- TIFF predictors combined with JPEG.
- JPEG-compressed tiles.

The fixtures and results below establish the tested scope. They do not establish compatibility with every JPEG or TIFF variant.

## 🔍 Milestone 1: Establishing a reproducible starting point

Before changing the implementation, I built the project and ran the existing tests successfully.

I then created a **16 × 16 grayscale TIFF** with JPEG compression and a uniform pixel value of 128.

The existing reader could:

- Read the TIFF metadata.
- Report the correct image dimensions.
- Recognise the compression as JPEG.

However, requesting image data raised:

```text
TIFF error (Unsupported feature): Unsupported compression
```

To check that this was specifically a decoding gap, I saved an uncompressed control from Pillow's decoded image.

- Pillow confirmed that the compressed image and control contained identical decoded pixels.
- The OCaml reader successfully loaded the control.
- A temporary probe checked that all 256 pixels were 128.

This gave me a small input whose metadata and expected output were already understood.

## 🧩 Milestone 2: Understanding shared JPEG tables

The initial fixture stored its JPEG tables separately from its image strip.

I inspected:

- TIFF compression tag **259**, with value **7**.
- The **`JPEGTables` tag, 347**.
- Strip offsets and byte counts.
- The start and end markers in the shared tables and image strip.

For this fixture:

- The shared tables contained **289 bytes**.
- The image strip contained **30 bytes**.
- Both streams had their own start-of-image and end-of-image markers.

As an investigation step, I combined the shared tables and strip into a standalone JPEG stream, removing the adjoining end/start markers.

The resulting **315-byte JPEG** decoded with Pillow to exactly the same pixels as the TIFF image.

The production decoder reads the shared tables into its table state before processing each strip; it does not depend on this experimental concatenation.

### Metadata changes

I added:

- The `JPEGTables` tag constructor.
- Numeric mappings between the constructor and tag 347.
- Its `UNDEFINED` field type.
- A pretty-printing case.
- An optional `jpeg_tables` accessor.

The accessor checks the field type and byte count. Its implementation also handles table bytes stored directly inside a BigTIFF directory entry, although the fixtures in this contribution do not separately validate that path.

## ⚙️ Milestone 3: Implementing the decoder

The decoder lives in `src/jpeg.ml` and uses the existing dependencies.

### Reading and validation

- Bounds-checked byte reading.
- Big-endian JPEG segment-length reading.
- Marker and segment processing.
- Frame and scan header validation.
- Checks for missing tables and unsupported coding processes.

JPEG multibyte values use big-endian order independently of the surrounding TIFF's byte order.

### Reconstructing image blocks

- Quantization and Huffman table parsing.
- Bit reading with JPEG byte stuffing.
- Canonical Huffman decoding.
- Signed coefficient reconstruction.
- DC prediction and AC zero-run handling.
- Zigzag coefficient placement.
- Dequantization.
- A floating-point inverse DCT.
- Pixel rounding and clamping to the 0–255 range.

### Processing scans

- Decoding and placing 8 × 8 blocks.
- Clipping padded block edges to the image dimensions.
- Checking scan padding bits.
- Reading restart markers in sequence.
- Resetting the DC predictor at restart boundaries.

## 🔗 Milestone 4: Connecting JPEG decoding to TIFF reading

I added a JPEG branch to the existing `read_data` compression dispatch in `src/tiff.ml`.

For each non-sparse JPEG strip, it:

1. Checks the sample count, bit depth and sample format.
2. Checks the grayscale photometric interpretation.
3. Rejects TIFF predictors.
4. Calculates the current strip's dimensions.
5. Supplies any shared tables to the JPEG decoder.
6. Returns the decoded buffer to the existing pixel-reading flow.

The strip height is calculated from that strip's expected output size. This allows the final strip to contain fewer rows than the preceding strips.

## 🛠️ Milestone 5: Reviewing shared-table behaviour

Review against the TIFF/JPEG technical note identified two cases that needed additional handling.

### Miscellaneous markers in shared tables

The shared-table reader now:

- Skips application and comment segments.
- Consumes a restart-interval segment without carrying its interval into the image stream.
- Skips arithmetic-conditioning segments in the shared-table stream.

Skipping arithmetic-conditioning metadata does not enable arithmetic-coded image decoding.

### Protecting shared table definitions

After loading `JPEGTables`, the decoder records which quantization, DC Huffman and AC Huffman slots are global.

- Strips may use these global tables.
- Strips may define tables in other available slots.
- Attempts to redefine a global table raise `Malformed_file`.
- Decoder table state is created afresh for each strip.

Regression tests check the specific redefinition errors so that an unrelated parsing failure cannot count as success.

## 🧪 Milestone 6: Fixtures and pixel validation

All images are synthetic. They were generated locally rather than downloaded from an external image collection.

Fixture generation used Python 3.13 and Pillow 13.0.0 with JPEG and libtiff support. Pillow is a development tool for fixture generation and reference decoding; it is not an OCaml runtime dependency.

### Fixture inventory

All filenames below are relative to `test/data/`.

| Fixture | Purpose | Expected result |
|---|---|---|
| `issue19-gray-jpeg.tiff` | Uniform 16 × 16 grayscale image with shared tables | Every pixel equals 128 |
| `issue19-quadrants-jpeg.tiff` | Four distinct 8 × 8 regions | Correct values and placement |
| `issue19-pattern-jpeg.tiff` | Patterned 19 × 13 image | Pixels agree with its reference within tolerance |
| `issue19-pattern-reference.tiff` | Uncompressed Pillow-decoded reference | Reference for the patterned image |
| `issue19-multistrip-jpeg.tiff` | Patterned 19 × 35 image | Correct decoding across three strips |
| `issue19-multistrip-reference.tiff` | Uncompressed Pillow-decoded reference | Reference for the multiple-strip image |
| `issue19-truncated-jpeg.tiff` | Strip with its end marker excluded | Raises `Malformed_file` |
| `issue19-restart-jpeg.tiff` | Complete JPEG stream with seven restart markers and no shared tables | Correct decoding and restart handling |
| `issue19-restart-reference.tiff` | Uncompressed Pillow-decoded reference | Reference for the restart-marker image |
| `issue19-shared-misc-jpeg.tiff` | Shared tables containing additional markers | Every pixel remains 128 |
| `issue19-redefine-quant-jpeg.tiff` | Strip redefines a global quantization table | Raises the expected redefinition error |
| `issue19-redefine-dc-jpeg.tiff` | Strip redefines a global DC Huffman table | Raises the expected redefinition error |
| `issue19-redefine-ac-jpeg.tiff` | Strip redefines a global AC Huffman table | Raises the expected redefinition error |

### Exact-value fixtures

The uniform image contains 256 pixels, each checked against 128.

The quadrant image is checked throughout these regions:

| Region | Rows | Columns | Expected value |
|---|---|---|---:|
| Top-left | 0–7 | 0–7 | 32 |
| Top-right | 0–7 | 8–15 | 96 |
| Bottom-left | 8–15 | 0–7 | 160 |
| Bottom-right | 8–15 | 8–15 | 224 |

Coordinates are zero-indexed.

### Patterned reference comparisons

The patterned source images use:

```python
(17 * x + 29 * y + 7 * x * y) % 256
```

JPEG encoding uses quality 90.

For each reference comparison:

1. Generate and encode the grayscale image.
2. Decode the compressed image with Pillow.
3. Save the decoded result as an uncompressed TIFF.
4. Load both files through the OCaml reader.
5. Compare every pixel.

The reference therefore represents the decoded JPEG output, including compression losses, rather than the original source pixels.

The OCaml comparisons allow an absolute difference of at most **two intensity levels per pixel** for differences between inverse-DCT implementations.

Array dimensions use row/column order:

- 19 × 13 image: `[| 13; 19 |]`.
- 19 × 35 image: `[| 35; 19 |]`.

### Multiple-strip fixture

The 19 × 35 fixture contains:

- First strip: 16 rows.
- Second strip: 16 rows.
- Final strip: 3 rows.

Its test compares all 665 pixels, including both strip boundaries.

### Restart-marker fixture

The restart fixture was generated as a standalone JPEG with:

```python
restart_marker_blocks=2
```

That complete stream was wrapped in a small classic TIFF directory as one strip, without tag 347.

Generation checks confirmed:

- A restart interval of two blocks.
- Seven restart markers.
- No shared `JPEGTables`.
- Identical Pillow-decoded pixels from the TIFF and standalone JPEG.

### Malformed and shared-table fixtures

The truncated fixture retains its TIFF directory but declares a strip length of 28 bytes instead of 30, excluding the JPEG end marker.

The shared-table regression fixtures were assembled from the uniform image's tables and strip:

- The miscellaneous-marker fixture adds APP, COM, DRI and DAC segments to the shared-table stream.
- The three redefinition fixtures insert an existing global table definition into the image strip.

Encoded bytes can vary with Pillow and codec versions. When regenerating fixtures, verify the metadata, strip layout, markers and decoded pixels rather than assuming byte-for-byte identity.

## ✅ Test results

The basic TIFF suite increased from **84 to 104 cases**.

The 20 additional cases cover ten JPEG scenarios, each running through both **Unix and Eio**:

1. Uniform grayscale decoding.
2. Quadrant placement.
3. Patterned pixels and uneven dimensions.
4. Multiple strips and a shorter final strip.
5. Truncated JPEG input.
6. Restart markers without shared tables.
7. Miscellaneous markers in shared tables.
8. Global quantization table redefinition.
9. Global DC Huffman table redefinition.
10. Global AC Huffman table redefinition.

The latest local basic-suite run completed successfully:

```text
Ran: 104 tests in: 0.26 seconds.
OK
```

The existing GeoTIFF, LZW and property-test suites also passed during development. No new JPEG property tests were added.

## ▶️ Running the checks

From the repository root, with the project's opam environment configured:

```bash
dune build @fmt --auto
dune build @all
dune runtest
git diff --check
```

To force execution of cached tests when needed:

```bash
dune runtest --force
```

The checked-in fixtures allow the OCaml tests to run without installing Pillow.

## 📚 Reference

- [TIFF Technical Note #2 — JPEG-in-TIFF](https://libtiff.gitlab.io/libtiff/specification/technote2.html)

The implementation and tests focus on a defined grayscale subset. Support for additional JPEG processes, colour images and tiled layouts requires further implementation and validation.