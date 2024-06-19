open Compile_to_wasm_from_scratch

let compile filename =
  let chan = Stdio.In_channel.create filename in
  let lexbuf = Lexing.from_channel chan in
  let prog = Parser.prog Lexer.read lexbuf in
  Stdio.In_channel.close chan;
  let wat = Codegen.compile_prog prog in
  Sexplib.Sexp.output_hum Stdio.stdout wat
;;

let filename_param = Command.Param.(anon ("filename" %: string))

let command =
  Command.basic
    ~summary:"Compile a program to WebAssembly"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () -> compile filename))
;;

let () = Command_unix.run command
