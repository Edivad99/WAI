module Wai.Ast

type Expr =
  | Constant of int
  | Variable of string
  | Boolean of bool
  | UnOp of string * Expr
  | BinOp of Expr * string * Expr

type Stm =
  | VarDec of string * Expr
  | VarAss of string * Expr
  | Skip
  | IfThenElse of Expr * Stm * Stm option
  | While of Expr * Stm
  | Seq of Stm * Stm

module Utils =

  let opposite operator =
    match operator with
    | "<" -> ">="
    | "<=" -> ">"
    | ">" -> "<="
    | ">=" -> "<"
    | "==" -> "!="
    | "!=" -> "=="

    | "&&" -> "||"
    | "||" -> "&&"
    | s -> failwith $"{s} not a valid operator"