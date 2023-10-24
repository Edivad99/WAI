open System.Diagnostics
open FSharp.Text.Lexing
open Wai.AbstractState
open Wai.Domains.IntervalDomain
open Wai.Gui

let evaluate input =
  LexBuffer<char>.FromString input |> Parser.prog Lexer.tokenstream

[<EntryPoint>]
let main args =
  let input =
    """
var x = 0;
var y = 10;
while (x < 40) {
  x = x + 1;
  y = y + x;
}
    """

  let domain = IntervalDomain()
  let program = evaluate input

  let abstract_state = AbstractState(domain)

  let start = Stopwatch.GetTimestamp()
  let result, program_points = abstract_state.eval program
  let delta = Stopwatch.GetElapsedTime start

  let path = Report.generate_report input program_points delta

  Process.Start(ProcessStartInfo(UseShellExecute = true, FileName = path)) |> ignore
  0
