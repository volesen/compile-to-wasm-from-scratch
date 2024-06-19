open Base
open Sexplib.Sexp
open Syntax

let rec num_lets expr =
  match expr with
  | IntLit _ | Var _ -> 0
  | UnOp (_, e) -> num_lets e
  | BinOp (_, e1, e2) -> num_lets e1 + num_lets e2
  | If (e1, e2, e3) -> num_lets e1 + num_lets e2 + num_lets e3
  | Seq (e1, e2) -> num_lets e1 + num_lets e2
  | Call (_, args) -> List.fold args ~init:0 ~f:(fun acc e -> acc + num_lets e)
  | Let (_, e1, e2) -> 1 + num_lets e1 + num_lets e2
;;

let empty_env = Map.empty (module String)

let compile_unop op : Sexp.t list =
  match op with
  | Neg -> [ Atom "i32.neg" ]
;;

let compile_binop op : Sexp.t list =
  match op with
  | Add -> [ Atom "i32.add" ]
  | Sub -> [ Atom "i32.sub" ]
  | Mul -> [ Atom "i32.mul" ]
  | Div -> [ Atom "i32.div_s" ]
;;

let rec compile_expr ftab vtab expr : Sexp.t list =
  match expr with
  | IntLit i -> [ Atom "i32.const"; Atom (Int.to_string i) ]
  | Var x -> [ Atom "local.get"; Atom (Int.to_string (Map.find_exn vtab x)) ]
  | UnOp (op, e) -> compile_expr ftab vtab e @ compile_unop op
  | BinOp (op, e1, e2) ->
    compile_expr ftab vtab e1 @ compile_expr ftab vtab e2 @ compile_binop op
  | If (e1, e2, e3) ->
    compile_expr ftab vtab e1
    @ [ List
          [ Atom "if"
          ; List [ Atom "result"; Atom "i32" ]
          ; List ([ Atom "then" ] @ compile_expr ftab vtab e2)
          ; List ([ Atom "else" ] @ compile_expr ftab vtab e3) (* ; Atom "end" *)
          ]
      ]
  | Seq (e1, e2) ->
    compile_expr ftab vtab e1 @ [ Atom "drop" ] @ compile_expr ftab vtab e2
  | Call (f, args) ->
    let f = Map.find_exn ftab f in
    List.concat_map args ~f:(compile_expr ftab vtab)
    @ [ Atom "call"; Atom (Int.to_string f) ]
  | Let (x, e1, e2) ->
    let vtab' = Map.set vtab ~key:x ~data:(Map.length vtab) in
    compile_expr ftab vtab e1
    @ [ Atom "local.set"; Atom (Int.to_string (Map.find_exn vtab' x)) ]
    @ compile_expr ftab vtab' e2
;;

let compile_decl ftab name params body : Sexp.t =
  let vtab =
    List.foldi params ~init:empty_env ~f:(fun i env x -> Map.set env ~key:x ~data:i)
  in
  let num_locals = num_lets body in
  List
    ([ Atom "func"; List [ Atom "export"; Atom ("\"" ^ name ^ "\"") ] ]
     @ List.map params ~f:(fun _ -> List [ Atom "param"; Atom "i32" ])
     @ [ List [ Atom "result"; Atom "i32" ] ]
     @ List.init num_locals ~f:(fun _ -> List [ Atom "local"; Atom "i32" ])
     @ compile_expr ftab vtab body)
;;

let compile_prog decls : Sexp.t =
  let ftab =
    List.foldi decls ~init:empty_env ~f:(fun i env (name, _, _) ->
      Map.set env ~key:name ~data:i)
  in
  List
    ([ Atom "module" ]
     @ List.map decls ~f:(fun (name, params, body) -> compile_decl ftab name params body)
    )
;;
