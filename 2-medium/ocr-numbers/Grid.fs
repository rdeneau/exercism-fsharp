namespace OcrDigits

type Grid = Grid of R1: Row * R2: Row * R3: Row * R4: Row
and  Row  = Row of C1: char * C2: char * C3: char

module Grid =
    [<Literal>]
    let private gridWidth = 3

    [<Literal>]
    let private gridHeight = 4

    let private tryParseRow (row: string) : Row option =
        match row |> List.ofSeq with
        | [ c1; c2; c3 ] -> Some (Row (c1, c2, c3 ))
        | _ -> None

    let tryParseGrid (grid: string list) : Grid option =
        match grid |> List.map tryParseRow with
        | [ Some r1; Some r2; Some r3; Some r4 ] ->
            Some (Grid (r1, r2, r3, r4))
        | _ -> None

    let tryParseLineOfGrids (line: string list) : Grid list option =
        match line |> List.map String.length |> List.distinct with
        | [length] when length % gridWidth = 0 ->
            [ for gridPosition in 0 .. gridWidth .. length-1 do
                line
                |> List.map (fun row -> row.Substring(gridPosition, gridWidth))
                |> tryParseGrid ] // Grid option list
            |> List.sequence
        | _ -> None

    let tryParseLinesOfGrids (rows: string list) : Grid list list option =
        rows
        |> List.chunkBySize gridHeight
        |> List.map tryParseLineOfGrids // Grid list option list
        |> List.sequence
