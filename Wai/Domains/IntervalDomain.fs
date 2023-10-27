module Wai.Domains.IntervalDomain

open Wai.Domains.Domain
open Wai.Ast

type Number =
  | MinusInf
  | Num of int
  | PlusInf

  static member (+)(x, y) =
    match x, y with
    | Num l, Num r -> Num(l + r)

    | Num _, PlusInf
    | PlusInf, Num _ -> PlusInf

    | Num _, MinusInf
    | MinusInf, Num _ -> MinusInf

    | PlusInf, PlusInf -> PlusInf
    | MinusInf, MinusInf -> MinusInf
    | _ -> failwith "sum operands not supported"

  static member (-)(x, y) =
    match x, y with
    | Num l, Num r -> Num(l - r)

    | Num _, PlusInf -> MinusInf
    | Num _, MinusInf -> PlusInf

    | MinusInf, Num _ -> MinusInf
    | MinusInf, PlusInf -> MinusInf

    | PlusInf, Num _ -> PlusInf
    | PlusInf, MinusInf -> PlusInf
    | _ -> failwith "minus operands not supported"

  static member (*)(x, y) =
    match x, y with
    | Num l, Num r -> Num(l * r)

    | _, Num z
    | Num z, _ when z = 0 -> Num 0 // 0 x _ = 0

    | Num n, PlusInf
    | PlusInf, Num n -> if n < 0 then MinusInf else PlusInf

    | Num n, MinusInf
    | MinusInf, Num n -> if n < 0 then PlusInf else MinusInf

    | MinusInf, MinusInf
    | PlusInf, PlusInf -> PlusInf
    | PlusInf, MinusInf
    | MinusInf, PlusInf -> MinusInf

  static member (/)(x, y) =
    match x, y with
    | Num num, Num den ->
      match num, den with
      | 0, _ -> Num 0
      | _, 0 -> if num < 0 then MinusInf else PlusInf
      | _ -> Num(num / den)
    | Num _, PlusInf
    | Num _, MinusInf -> Num 0 // n / (+-Inf) = 0
    | PlusInf, Num n -> if n >= 0 then PlusInf else MinusInf
    | MinusInf, Num n -> if n >= 0 then MinusInf else PlusInf
    | _ -> Num 0 // (+-Inf) / (+-Inf) = 0

  override this.ToString() =
    match this with
    | PlusInf -> "+\u221E"
    | Num n -> n.ToString()
    | MinusInf -> "-\u221E"

type Interval =
  | Range of Number * Number
  | Bottom

  static member (~-) x =
    match x with
    | Range(n1, n2) ->
      match n1, n2 with
      | Num a, Num b -> Range(Num -b, Num -a)
      | MinusInf, Num a -> Range(Num -a, PlusInf)
      | Num a, PlusInf -> Range(MinusInf, Num -a)
      | _ -> Range(MinusInf, PlusInf) // [+-Inf, +-Inf] = [-Inf, +Inf] Always sound
    | Bottom -> Bottom

  static member (+)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) -> Range(a + c, b + d)
    | _ -> Bottom

  static member (-)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) -> Range(a - d, b - c)
    | _ -> Bottom

  static member (*)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) ->
      let ac = a * c
      let ad = a * d
      let bc = b * c
      let bd = b * d

      let min = List.min [ ac; ad; bc; bd ]
      let max = List.max [ ac; ad; bc; bd ]
      Range(min, max)
    | _ -> Bottom

  static member union x y =
    match x, y with
    | Range(a, b), Range(c, d) -> Range(min a c, max b d)
    | Bottom, _ -> y
    | _, Bottom -> x

  static member intersect x y =
    match x, y with
    | Range(a, b), Range(c, d) ->
      let lower = max a c
      let higher = min b d
      if lower <= higher then Range(lower, higher) else Bottom
    | _ -> Bottom

  static member (/)(x, y) =
    match x, y with
    | Range(a, b), Range(c, d) ->
      if c = Num 0 && d = Num 0 then
        Bottom
      elif Num 1 <= c then
        let ac = a / c
        let ad = a / d
        let bc = b / c
        let bd = b / d

        let min = List.min [ ac; ad; bc; bd ]
        let max = List.max [ ac; ad; bc; bd ]
        Range(min, max)
      elif d <= Num -1 then
        let t1 = -x
        let t2 = -y
        t1 / t2 // [-b, -a] / [-d, -c] if d <= 0
      else
        let t1 = x / Interval.intersect y (Range(Num 1, PlusInf))
        let t2 = x / Interval.intersect y (Range(MinusInf, Num -1))
        Interval.union t1 t2
    | _ -> Bottom

  override this.ToString() =
    match this with
    | Range(l, r) -> $"[{l.ToString()}, {r.ToString()}]"
    | Bottom -> "\u22A5"

type IntervalDomain() =
  inherit Domain<Interval>()

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

  member private this.eval_expr expr state =
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

  override this.eval_var_dec var_name expr state =
    match state.ContainsKey var_name with
    | true -> state.Add(var_name, Bottom)
    | false ->
      let value = this.eval_expr expr state
      state.Add(var_name, value)

  override this.eval_var_ass var_name expr state =
    match state.ContainsKey var_name with
    | false -> state.Add(var_name, Bottom)
    | true ->
      let value = this.eval_expr expr state
      state.Add(var_name, value)

  override this.eval_leq l r state =
    match l, r with
    | Constant a, Constant b -> if a <= b then state else Map.empty
    | Variable var_name, Constant c ->
      let c = Num c

      match this.eval_expr l state with
      | Range(a, _) when c < a -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(a, min b c))
      | _ -> state
    | Constant c, Variable var_name ->
      let c = Num c

      match this.eval_expr r state with
      | Range(_, b) when b < c -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(max a c, b))
      | _ -> state

    | Variable left_var_name, Variable right_var_name ->
      match this.eval_expr l state, this.eval_expr r state with
      | Range(a, _), Range(_, d) when a > d -> Map.empty
      | Range(a, b), Range(c, d) ->
        state
          .Add(left_var_name, Range(a, min b d))
          .Add(right_var_name, Range(max c a, d))
      | _ -> state
    | _ -> state

  override this.eval_grt l r state =
    match l, r with
    | Constant a, Constant b -> if a > b then state else Map.empty
    | Variable var_name, Constant c ->
      let c = Num c

      match this.eval_expr l state with
      | Range(a, b) when c >= b -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(max a (c + Num 1), b))
      | _ -> state
    | Constant c, Variable var_name ->
      let c = Num c

      match this.eval_expr r state with
      | Range(a, b) when c <= a -> Map.empty
      | Range(a, b) -> state.Add(var_name, Range(a, min (c - Num 1) b))
      | _ -> state
    | Variable left_var_name, Variable right_var_name ->
      match this.eval_expr l state, this.eval_expr r state with
      | Range(a, b), Range(c, d) when b <= c -> Map.empty
      | Range(a, b), Range(c, d) ->
        state
          .Add(left_var_name, Range(max a (c + Num 1), b))
          .Add(right_var_name, Range(c, min (b - Num 1) d))
      | _ -> state
    | _ -> state

  override this.eval_equ l r state =
    let left_val = this.eval_expr l state
    let right_val = this.eval_expr r state

    match left_val, right_val with
    | Range(a, b), Range(c, d) when b < c || d < a -> Map.empty
    | Range _, Range _ ->
      let new_value = this.intersect left_val right_val

      match l, r with
      | Variable left_var_name, Variable right_var_name -> state.Add(left_var_name, new_value).Add(right_var_name, new_value)
      | Variable left_var_name, _ -> state.Add(left_var_name, new_value)
      | _, Variable right_var_name -> state.Add(right_var_name, new_value)
      | _ -> state
    | _ -> state

  override this.eval_neq l r state =
    match l, r with
    | Constant a, Constant b -> if a <> b then state else Map.empty
    | Variable var_name, Constant c
    | Constant c, Variable var_name ->
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
      | _ -> state

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
      | _ -> state
    | _ -> state