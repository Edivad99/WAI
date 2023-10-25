module Ast.Program

open Wai
open Wai.AbstractState
open Wai.Domains.IntervalDomain

[<EntryPoint>]
let main args =
  let input =
    """
skip;
var x = 2;
if (x == 2) {
  skip;
  x = x + 1;
}
while (x < 2) {
  skip;
}
if (x < 2) {
  skip;
  x = x + 1;
} else {
  var y = 10
}
"""

  let program = Evaluate.evaluate input
  printfn $"{program}"

  let domain = IntervalDomain()
  let abstract_state = AbstractState(domain)

  let result, program_points = abstract_state.eval program
  printfn $"{result}"
  0
