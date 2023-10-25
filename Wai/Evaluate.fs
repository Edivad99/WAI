module Wai.Evaluate

open FSharp.Text.Lexing

let evaluate input =
  let lexbuf = LexBuffer<char>.FromString input
  Parser.prog Lexer.tokenstream lexbuf