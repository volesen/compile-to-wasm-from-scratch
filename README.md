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
