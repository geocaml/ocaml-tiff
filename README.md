# ocaml-tiff

A pure OCaml library for reading TIFF files. The underlying IO mechanisms are
expected to be provided by the user using a library of their choice. For
example, you could use [Eio](https://github.com/ocaml-multicore/eio).

## Installing

`ocaml-tiff` is not yet released, until it has been you will need to pin it to use it.

```
opam pin -yn git+https://github.com/geocaml/ocaml-tiff
opam install tiff
```

## Tests & Benchmarks

To run the test suite, you can run `dune runtest`. To run the benchmarks use
`dune build @runbench -f` (the `-f` will run the benchmarks unconditionally of
whether the required dependencies have changed).


