# Contributing to ocaml-tiff

To contribute to ocaml-tiff you will need a working OCaml toolchain. For now,
this project uses [opam](https://opam.ocaml.org).  

Follow the instructions at [Installing OCaml](https://ocaml.org/install). Once
you have opam installed, fork and clone this repository and create a fresh
switch.

```
git clone https://github.com/<username>/ocaml-tiff
cd ocaml-tiff
opam switch create . --deps-only --with-test --with-dev-setup
```

This may take a moment to build the OCaml compiler and all the dependencies for
this project. You may wish to also install `ocaml-lsp-server` and [configure
your editor](https://ocaml.org/docs/set-up-editor) for a better developer
experience.

## Building

To build the project, run the `dune build` command. All the code is located
under the `src` directory.

## Testing

The tests are stored under the `test` directory. Additional data can be stored
in `test/data`.

To test the codebase run `dune test`. 

## Formatting

The codebase uses `ocamlformat` to format the code. Before committing please
run `dune build @fmt --auto` to reformat your code into a consistent style.
