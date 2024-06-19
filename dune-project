(lang dune 3.15)

(name compile_to_wasm_from_scratch)

(generate_opam_files true)

(source
 (github volesen/compile-to-wasm-from-scratch))

(authors "volesen")

(maintainers "volesen")

(license LICENSE)

(documentation https://github.com/volesen/compile-to-wasm-from-scratch)

(package
 (name compile_to_wasm_from_scratch)
 (synopsis "A compiler for a toy language to WebAssembly")
 (description "A compiler for a toy language to WebAssembly")
 (depends ocaml dune)
 (tags
  (wasm compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
(using menhir 3.0)