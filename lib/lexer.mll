{
  open Parser

  exception LexError of string
}
 
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+  (* TODO: Test unary minus *)
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse 
  | whitespace { read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "def" { DEF }
  | "," { COMMA }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (LexError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
