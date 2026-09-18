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

## AI and LLMs

All uses of AI/LLMs for composing or processing prose must be declared either
at the start or the end of the prose. Ideally, if the contributor feels they
can, they should not use these tools at all for generating prose.

AI-generated code will not be used in production, to quote [Jon
Sterling](https://www.jonmsterling.com/0LWG/):

> Use your best judgement and do not try to be a lawyer about this: there’s a
> qualitative difference between autoformalisation and getting an LLM to figure
> out how to phrase a complicated `match...as...in...return...with...end`
> expression.

Using these tools to help you think or debug a problem is okay provided you
come to a full understanding of the code and the problem yourself.
