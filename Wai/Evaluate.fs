module Wai.Evaluate

open System
open System.IO
open System.Reflection
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

let file_read input =
  Assembly.GetExecutingAssembly().Location
  |> Path.GetDirectoryName
  |> fun x -> Path.Combine (x, "Examples", input)
  |> File.ReadAllText
