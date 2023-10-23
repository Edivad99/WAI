module Program

open FSharp.Text.Lexing

let evaluate input =
  let lexbuf = LexBuffer<char>.FromString input
  Parser.prog Lexer.tokenize lexbuf


[<EntryPoint>]
let main args =
  let input = """
skip;
var x = 2;
if (x < 2) {
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
  0