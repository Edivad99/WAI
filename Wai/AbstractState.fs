module Wai.AbstractState

open Wai.Ast
open Wai.Domains.Domain

type AbstractState<'T when 'T: equality>(domain: Domain<'T>) =
  member _.Domain = domain

  member private this.eval(program: Stm, state: Map<string, 'T>, state_points: Map<string, 'T> list) =
    match program with
    | Skip -> (state, state_points @ [ state ])

    | VarDec(var_name, expr) ->
      let state = this.Domain.eval_var_dec var_name expr state
      (state, state_points @ [ state ])

    | VarAss(var_name, expr) ->
      let state = this.Domain.eval_var_ass var_name expr state
      (state, state_points @ [ state ])

    | Seq(p1, p2) ->
      let (s1, state_points) = this.eval (p1, state, state_points)
      this.eval (p2, s1, state_points)

    | IfThenElse(cond, true_branch, false_branch) ->
      let state_cond = this.Domain.eval_abstr_cond cond state

      let s1, true_branch_points =
        if Map.isEmpty state_cond then
          (Map.empty, [ Map.empty ])
        else
          this.eval (true_branch, state_cond, List.empty)

      match false_branch with
      | Some false_branch ->
        let state_else_cond = this.Domain.eval_abstr_cond (UnOp("!", cond)) state

        let s2, false_branch_points =
          if Map.isEmpty state_else_cond then
            (Map.empty, [ Map.empty ])
          else
            this.eval (false_branch, state_else_cond, List.empty)

        let next = this.Domain.point_wise_union s1 s2

        (next,
         state_points
         @ [ state_cond ]
           @ true_branch_points @ [ state_else_cond ] @ false_branch_points @ [ next ])
      | None ->
        let next = this.Domain.point_wise_union s1 state

        (next, state_points @ [ state_cond ] @ true_branch_points @ [ next ])

    | While(cond, expr) ->
      let mutable prev_state = this.Domain.eval_abstr_cond cond state

      let mutable before_body = []
      let mutable after_body = []
      let mutable fixpoint = false
      let mutable iteration = 1

      while not fixpoint do
        let s2 = this.Domain.eval_abstr_cond cond prev_state
        // Salva i program points
        before_body <- [ s2 ]
        // Valuta il corpo del while con gli stati che soddisfano la condizione
        let while_state, body_points = this.eval (expr, s2, List.empty)
        after_body <- body_points
        // Fai l'unione point wise tra gli stati originali e gli ultimi
        let union = this.Domain.point_wise_union state while_state
        // Applica il widening per accelerare la divergenza
        let curr_state = this.Domain.point_wise_widening prev_state union
        // Controlla se abbiamo l'invariante
        fixpoint <- prev_state = curr_state
        // Aggiorna il prev_state
        prev_state <- curr_state
        iteration <- iteration + 1

      let state_after_while = this.Domain.eval_abstr_cond (UnOp("!", cond)) prev_state

      let state_after_while_narr =
        this.Domain.point_wise_narrowing state_after_while (List.last after_body)

      (state_after_while_narr, state_points @ before_body @ after_body @ [ state_after_while_narr ])

  member this.eval program =
    this.eval (program, Map.empty, [ Map.empty ])
