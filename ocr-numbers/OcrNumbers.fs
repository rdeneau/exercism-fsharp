module OcrNumbers

open OcrDigits

let convert (rows: string list) : string option =
    rows
    |> Grid.tryParseLinesOfGrids // Grid list list option
    |> Option.map (fun line ->
        line
        |> List.map (fun grids ->
            grids
            |> List.map (Digit.recognize >> Digit.char >> string)
            |> String.concat "" )
        |> String.concat "," )
