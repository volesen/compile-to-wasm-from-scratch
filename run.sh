# #!/bin/bash
TMP=$(mktemp)

dune exec compile_to_wasm_from_scratch -- $1 > $TMP.wat
wat2wasm $TMP.wat -o $TMP.wasm
wasm-interp $TMP.wasm --run-all-exports