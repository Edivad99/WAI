module Wai.Domains.Domain

open Wai.Ast
open Wai.Ast.Utils

[<AbstractClass>]
type Domain<'T when 'T: equality>() =

  abstract eval_var_dec: var_name: string -> expr: Expr -> state: Map<string, 'T> -> Map<string, 'T>
  abstract eval_var_ass: var_name: string -> expr: Expr -> state: Map<string, 'T> -> Map<string, 'T>

  abstract eval_abstr_cond: expr: Expr -> state: Map<string, 'T> -> Map<string, 'T>

  abstract union: x: 'T -> y: 'T -> 'T
  abstract intersect: x: 'T -> y: 'T -> 'T
  abstract widening: x: 'T -> y: 'T -> 'T
  abstract narrowing: x: 'T -> y: 'T -> 'T

  member private _.resolve_conflicts f acc key value =
    match Map.tryFind key acc with
    | Some v -> Map.add key (f v value) acc
    | None -> Map.add key value acc

  member this.point_wise_union (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
    Map.fold (this.resolve_conflicts this.union) s1 s2

  member this.point_wise_widening (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
    Map.fold (this.resolve_conflicts this.widening) s1 s2

  member this.point_wise_narrowing (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
    Map.fold (this.resolve_conflicts this.narrowing) s1 s2

  member this.point_wise_intersection (s1: Map<string, 'T>) (s2: Map<string, 'T>) =
    Map(
      seq {
        for KeyValue(k, vs1) in s1 do
          match Map.tryFind k s2 with
          | Some vs2 -> yield k, this.intersect vs1 vs2
          | None -> ()
      }
    )

  member this.eval_generic_abstr_cond expr state =
    match expr with
    | Boolean true -> state
    | Boolean false -> Map.empty

    | BinOp(l, "<", r) -> this.eval_abstr_cond (BinOp(r, ">", l)) state
    | BinOp(l, ">=", r) -> this.eval_abstr_cond (BinOp(r, "<=", l)) state

    | BinOp(l, "&&", r) ->
      let left_val = this.eval_abstr_cond l state
      let right_val = this.eval_abstr_cond r state
      this.point_wise_intersection left_val right_val

    | BinOp(l, "||", r) ->
      let left_val = this.eval_abstr_cond l state
      let right_val = this.eval_abstr_cond r state
      this.point_wise_union left_val right_val

    | UnOp("!", expr) ->
      match expr with
      | Boolean true -> Map.empty
      | Boolean false -> state

      | BinOp(l, ("&&" | "||" as op), r) ->
        let not_l = UnOp("!", l)
        let not_r = UnOp("!", r)
        let opposite = opposite op
        this.eval_abstr_cond (BinOp(not_l, opposite, not_r)) state

      | BinOp(l, op, r) ->
        let opposite = opposite op
        this.eval_abstr_cond (BinOp(l, opposite, r)) state

      | UnOp("!", expr) -> this.eval_abstr_cond expr state
      | _ -> state
    | _ -> state
