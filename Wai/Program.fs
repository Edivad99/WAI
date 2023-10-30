module Wai.Program

open Wai.AbstractState
open Wai.Domains.Interval.IntervalDomain


[<EntryPoint>]
let main args =

  let program = Evaluate.file_read "input.wl" |> Evaluate.evaluate
  printfn $"{program}"

  let domain = IntervalDomain()
  let abstract_state = AbstractState(domain)
  let result, program_points = abstract_state.eval program
  printfn $"Result: {result}"
  0
