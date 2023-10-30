module WaiGui.Report

open System
open System.IO
open System.Reflection
open HandlebarsDotNet

let private source =
  """<!DOCTYPE html>
    <html lang="en" data-bs-theme="dark">
    <head>
      <meta charset="UTF-8">
      <title>WAI</title>
      <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
      <style>
        pre {
          background: #161b22;
          width: max-content;
          border-radius: 15px;
          padding: 15px;
          margin-top: 15px;
        }
      </style>
    </head>
    <body style="margin-left: 20px; background: #0d1117">
      <h3 class="display-3">While Abstract Interpreter</h3>
      <strong>Davide Albiero, Damiano Mason</strong>
      <br>
      <small>Analyzed in: {{delta}} ms</small><br>
      <small>Generated at: {{time}}</small>
      <pre>{{{code}}}</pre>
    </body>
    </html>"""

let private template = Handlebars.Compile(source)

let rec private interleave (program_points: string list, code_lines: string list) =
  match program_points, code_lines with
  | ([], code_lines) ->
    printfn "warning: interleave"
    code_lines //NON DOVREBBE MAI SUCCEDERE
  | (program_points, []) -> program_points
  | (program_point :: program_points, code_line :: code_lines) ->
    // Ignoriamo i commenti
    if (code_line.StartsWith("//")) then
      code_line :: interleave (program_point :: program_points, code_lines)
    else
      let spaces = Seq.length (Seq.takeWhile Char.IsWhiteSpace code_line)
      let program_point = (String.replicate spaces " ") + program_point
      program_point :: code_line :: interleave (program_points, code_lines)

let private pretty_points points =
  if Map.isEmpty points then
    "Bottom"
  else
    points
    |> Map.toList
    |> List.map (fun (x, y) -> $"{x.ToString()}:{y.ToString()}")
    |> String.concat "; "

let private format_code (code: string, program_points: Map<string, 'a> list) =
  let formatted_code =
    code.Split '\n'
    //|> Array.map (fun x -> x.Replace ("        ", ""))
    |> Array.filter (fun x -> not (String.IsNullOrWhiteSpace x))
    //|> Array.filter (fun x -> not (x.StartsWith("//")))
    |> List.ofArray

  let formatted_program_points =
    program_points
    |> List.map pretty_points
    |> List.map (fun x -> $"""<span style="color: var(--bs-success)">// {x}</span>""")

  interleave (formatted_program_points, formatted_code) |> String.concat ("\n")

let generate_report code program_points (delta: TimeSpan) =
  let data =
    {| code = format_code (code, program_points)
       time = DateTime.Now.ToString()
       delta = delta.Milliseconds |}

  let result = template.Invoke(data)

  let executableLocation =
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

  let file_path = Path.Combine(executableLocation, "report.html")
  File.WriteAllText(file_path, result)
  file_path
