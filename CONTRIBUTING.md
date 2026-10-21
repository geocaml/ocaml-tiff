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

## Repository structure:

### Root repository folder structure:

Here are the 5 folders in the root directory. Please go through the table below to find out which folder does what:

| Sr. No. | Folder name | Folder Content | What does the folder do? (Layman edition/Newcomer edition) | What does these file do? |
| :---- | :---- | :---- | :---- | :---- |
| 1 | **\`src/\`** | the main source code | Builds the functionality | Contains the OCaml code that implements the library. This is where you will find the logic responsible for understanding TIFF files and their contents. |
| 2 | **\`test/\`** | tests | Checks that the functionality works | Contains code that checks whether the library works correctly, along with sample TIFF files used for testing. Tests help catch errors when the code is changed. |
| 3 | **\`examples/\`** | example programs | Shows how to use it | Demonstrates how the library can be used by another program. These examples can be easier to understand than the implementation itself. |
| 4 | **\`bench/\`** | benchmarks | Measures its performance | Code for measuring performance. For example, how quickly the library reads or decodes image data. |
| 5 | **\`fuzz/\`** | robustness testing | Helps uncover problems with unusual inputs | Supports testing the code with varied or unexpected inputs to find crashes and parsing errors. |

### Root repository file structure:

The root directory consists of some more important files. Check these out below:

| Sr. No. | File | What it does? |
| ----- | ----- | ----- |
| 1 | **`` `README.md` ``** | The introduction: what the project does and how to run tests or benchmarks. |
| 2 | **`` `HACKING.md` ``** | Technical notes explaining the internals and development details. |
| 3 | **`` `CONTRIBUTING.md` ``** | Instructions for developers who want to contribute. |
| 4 | **`` `dune-project` ``** | Defines the project and its package configuration, including dependencies. |
| 5 | **`` `dune` ``** | Configures build behaviour at the repository root. |
| 6 | **`` `tiff.opam` ``** | Package metadata and dependency requirements for OCaml's package manager. |
| 7 | **`` `tiff.opam.template` ``** | A template used in generating or maintaining package metadata. |
| 8 | **`` `.ocamlformat` ``** | Controls the formatting style used for OCaml code. |
| 9 | **`` `.gitignore` ``** | Lists generated files or other items Git should ignore. |
| 10 | **`` `.gitattributes` ``** | Defines how Git handles selected file types. |
| 11 | **`` `LICENSE` ``** | Describes the legal terms under which the code may be used |

### File extensions meaning:

This is one of the most useful things to learn first. You do not need to understand the code inside a file to recognise its purpose.

| Sr. No. | File type | Meaning | Familiar comparison with a Python project |
| ----- | ----- | ----- | ----- |
| `1` | **`` `.ml` ``** | OCaml implementation: the actual code and logic | A `.py` file in Python or an `.R` file in R |
| `2` | **`` `.mli` ``** | OCaml interface: declares what functions, types and modules are available to other parts of the program | Similar in purpose to a public API declaration or a header file |
| `3` | **`` `.md` ``** | Markdown documentation: explanations, instructions and project notes | A GitHub README |
| `4` | **`` `.tiff` ``** | An actual image file, often used as test input | A sample dataset |
| `5` | **`` `.opam` ``** | Metadata and dependency information for the OCaml package manager | Similar in purpose to a package manifest |
| `6` | **`` `.ocamlformat` ``** | Rules for formatting OCaml source code | Similar to a Python formatter configuration |
| `7` | **`` `.gitignore` ``** | Files and folders Git should generally ignore | The same concept in a Python project or any Git repository |
| `8` | **`` `.gitattributes` ``** | Rules for how Git handles particular files | Git-level file handling settings |
