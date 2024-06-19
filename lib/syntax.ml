type id = string

type unop =
  | Neg
  | Not

type binop =
  | Add
  | Sub
  | Mul
  | Div

type expr =
  | IntLit of int32
  | Var of id
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | If of expr * expr * expr
  | Let of id * expr * expr
  | Seq of expr * expr
  | Call of id * expr list

type decl = id * id list * expr
type prog = decl list
