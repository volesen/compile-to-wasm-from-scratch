%{
  open Syntax
%}
%token <int> INT
%token <string> ID
%token SEMICOLON
%token EQUAL
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token DEF
%token COMMA
%token EOF

%right SEMICOLON
%nonassoc IN
%nonassoc ELSE
%right EQUAL
%left PLUS MINUS
%left STAR SLASH

%start <prog> prog

%%

prog:
  | decls=list(decl); EOF; { decls }

decl:
  | DEF; name=ID; LPAREN; args=separated_list(COMMA, ID); RPAREN; EQUAL; body=expr; { (name, args, body) }

expr:
  | n=INT; { IntLit n }
  | x=ID; { Var x }
  | op=unop; e=expr; { UnOp (op, e) }
  | e1=expr; op=binop; e2=expr; { BinOp (op, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If (e1, e2, e3) }
  | LET; name=ID; EQUAL; e1=expr; IN; e2=expr; { Let (name, e1, e2) }
  | LPAREN; e=expr; RPAREN; { e }
  | f=ID; LPAREN; args=separated_list(COMMA, expr); RPAREN; { Call (f, args) }
  | e1=expr; SEMICOLON; e2=expr; { Seq (e1, e2) }

%inline unop:
  | MINUS { Neg }

%inline binop:
  | EQUAL { Eq }
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
