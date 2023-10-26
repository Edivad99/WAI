module Wai.Evaluate

open System
open FSharp.Text.Lexing

let evaluate input =
  let lexbuf = LexBuffer<char>.FromString input
  try
    Parser.prog Lexer.tokenstream lexbuf
  with e ->
    let pos = lexbuf.EndPos
    printfn $"Parse failed at line {pos.Line}, column {pos.Column}"
    printfn $"Last token: {String(lexbuf.Lexeme)}"
    printfn $"Message: {e.Message}"
    exit 1