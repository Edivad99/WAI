module Ast.Program

open System.IO
open System.Reflection
open Wai
open Wai.AbstractState
open Wai.Domains.IntervalDomain


[<EntryPoint>]
let main args =
  let input =
    Assembly.GetExecutingAssembly().Location
    |> Path.GetDirectoryName
    |> fun x -> Path.Combine (x, "input.wl")
    |> File.ReadAllText

  let program = Evaluate.evaluate input
  printfn $"{program}"

  let domain = IntervalDomain()
  let abstract_state = AbstractState(domain)

  let result, program_points = abstract_state.eval program
  printfn $"{result}"
  0
