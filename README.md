# compile-to-wasm-from-scratch

A simple example of compiling a minimal language to WebAssembly from scratch.

## Example

```
def sum_to(n) =
    if n then
        n + sum_to(n - 1)
    else
        0
```

## Usage

Prerequisites:
- [OCaml](https://ocaml.org/install)
- [WebAssembly Binary Toolkit (`wabt`)](https://github.com/WebAssembly/wabt)

Install the dependencies:
```sh
opam install core
```

Run a `.ctwfs` file:
```sh
sh run.sh examples/sum_to.ctwfs
```