module Ast

type UnOperator =
  | Minus
  | Not

type BinOperator =
  | Sum
  | Subtraction
  | Multiplication
  | Division
  | And
  | Or
  | LessThan
  | LessEqualThan
  | GreaterThan
  | GreaterEqualThan
  | Equal
  | NotEqual

type Expr =
  | Constant of int
  | Variable of string
  | Boolean of bool
  | UnOp of UnOperator * Expr
  | BinOp of Expr * BinOperator * Expr
  | VarAsn of string * Expr

type Stm =
  | VarDec of string * Expr
  | VarAss of string * Expr
  | Skip
  | IfThenElse of Expr * Stm * Stm option
  | While of Expr * Stm
  | Seq of Stm list
