module Ast.Program

open FSharp.Text.Lexing
open Wai.AbstractState
open Wai.Domains.IntervalDomain

let evaluate input =
  let lexbuf = LexBuffer<char>.FromString input
  Parser.prog Lexer.tokenstream lexbuf


[<EntryPoint>]
let main args =
  let input = """
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
  var y = 10;
}
"""
  let program = evaluate input
  printfn $"{program}"
  
  let domain = IntervalDomain()
  let abstract_state = AbstractState(domain)
  
  let result, program_points = abstract_state.eval program
  0