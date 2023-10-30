module Wai.Domains.Interval.IntervalDomain

open Wai.Ast
open Wai.Domains.Domain
open Wai.Domains.Interval.Types

type IntervalDomain() =
  inherit Domain<Interval>()

  override _.bottom = Bottom
  override _.top = Range(MinusInf, PlusInf)

  override this.union x y = Interval.union x y

  override this.widening x y =
    match x, y with
    | Range(a, b), Range(c, d) ->
      let lower =
        if a <= c then a
        elif Num 0 <= c && c < a then Num 0
        else MinusInf

      let higher =
        if b >= d then b
        elif Num 0 >= d && d > b then Num 0
        else PlusInf

      Range(lower, higher)
    | Bottom, _ -> y
    | _, Bottom -> x

  override this.narrowing x y =
    match x, y with
    | Range(a, b), Range(c, d) ->
      let lower = if a = MinusInf then c else a
      let higher = if b = PlusInf then d else b
      Range(lower, higher)
    | Bottom, _ -> y
    | _, Bottom -> x

  override this.intersect x y = Interval.intersect x y

  override this.eval_expr expr state =
    match expr with
    | Constant value -> Range(Num value, Num value) // TODO: Crea un metodo che si occupa di creare Range
    | Variable var_name -> Map.tryFind var_name state |> Option.defaultValue Bottom
    | UnOp("-", expr) -> -this.eval_expr expr state
    | BinOp(l, ("+" | "-" | "*" | "/" as op), r) ->
      let left_val = this.eval_expr l state
      let right_val = this.eval_expr r state

      match op with
      | "+" -> left_val + right_val
      | "-" -> left_val - right_val
      | "*" -> left_val * right_val
      | "/" -> left_val / right_val
      | _ -> failwithf "Not implemented yet"
    | Expr.Range(a, b) -> Range(Num a, Num b)
    | _ -> failwithf "Not implemented yet"

  override this.eval_leq l r state =
    match l, r with
    | Constant a, Constant b -> if a <= b then state else Map.empty
    | Variable var_name, Constant c ->
      let c = Num c

      match this.eval_expr l state with
      | Range(a, _) when c < a -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(a, min b c))
      | Bottom -> Map.empty
    | Constant c, Variable var_name ->
      let c = Num c

      match this.eval_expr r state with
      | Range(_, b) when b < c -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(max a c, b))
      | Bottom -> Map.empty

    | Variable left_var_name, Variable right_var_name ->
      match this.eval_expr l state, this.eval_expr r state with
      | Range(a, _), Range(_, d) when a > d -> Map.empty
      | Range(a, b), Range(c, d) ->
        state
        |> Map.add left_var_name (Range(a, min b d))
        |> Map.add right_var_name (Range(max c a, d))
      | _ -> Map.empty
    | _ -> state

  override this.eval_grt l r state =
    match l, r with
    | Constant a, Constant b -> if a > b then state else Map.empty
    | Variable var_name, Constant c ->
      let c = Num c

      match this.eval_expr l state with
      | Range(_, b) when c >= b -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(max a (c + Num 1), b))
      | Bottom -> Map.empty
    | Constant c, Variable var_name ->
      let c = Num c

      match this.eval_expr r state with
      | Range(a, _) when c <= a -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(a, min (c - Num 1) b))
      | Bottom -> Map.empty
    | Variable left_var_name, Variable right_var_name ->
      match this.eval_expr l state, this.eval_expr r state with
      | Range(_, b), Range(c, _) when b <= c -> Map.empty
      | Range(a, b), Range(c, d) ->
        state
        |> Map.add left_var_name (Range(max a (c + Num 1), b))
        |> Map.add right_var_name (Range(c, min (b - Num 1) d))
      | _ -> Map.empty
    | _ -> state

  override this.eval_equ l r state =
    let left_val = this.eval_expr l state
    let right_val = this.eval_expr r state

    match left_val, right_val with
    | Range(a, b), Range(c, d) when b < c || d < a -> Map.empty
    | Range _, Range _ ->
      let new_value = this.intersect left_val right_val

      match l, r with
      | Variable left_var_name, Variable right_var_name ->
        state
        |> Map.add left_var_name new_value
        |> Map.add right_var_name new_value
      | Variable var_name, _
      | _, Variable var_name -> state.Add(var_name, new_value)
      | _ -> state
    | Bottom, _
    | _, Bottom -> Map.empty

  override this.eval_neq l r state =
    match l, r with
    | Constant a, Constant b -> if a <> b then state else Map.empty
    | Variable var_name, Constant c -> 
      let c = Num c
      let left_val = this.eval_expr l state

      match left_val with
      | Range(a, b) ->
        if c < a || c > b then
          state
        elif a < c && c < b then
          Map.empty
        elif a = c && c < b then
          state.Add(var_name, Range(a + Num 1, b))
        elif b = c && a < b then
          state.Add(var_name, Range(a, b - Num 1))
        else
          state
      | Bottom -> Map.empty
    | Constant _, Variable _ -> this.eval_neq r l state
    | Variable left_var_name, Variable right_var_name ->
      let left_val = this.eval_expr l state
      let right_val = this.eval_expr r state

      match left_val, right_val with
      | Range(a, b), Range(c, d) ->
        if c < a && b < d then
          Map.empty
        elif a < c && d < b then
          Map.empty
        elif a > d || b < c then
          state
        elif a > c then
          this.eval_abstr_cond (BinOp(r, "!=", l)) state
        else
          let state = state.Add(left_var_name, Range(a, min (c - Num 1) b))
          let lower_bound = min (max c (b + Num 1)) d
          state.Add(right_var_name, Range(lower_bound, d))
      | _ -> Map.empty
    | _ -> state