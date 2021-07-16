module WordSearch

type X = int
type Y = int
type Coordinate = X * Y

type Grid = string list

type GridA = { Lines: string array; Width: int; Height: int }

module GridA =
    let ofGrid (grid: Grid) : GridA =
        if grid.Length = 0 then
            { Lines  = [||]
              Width  = 0
              Height = 0 }
        else
            { Lines  = grid |> List.toArray
              Width  = grid.Head.Length
              Height = grid.Length }

type Direction = { Label: string; DeltaX: int; DeltaY: int }

let directions =
    [ { DeltaX = +0; DeltaY = -1; Label = "Top" }
      { DeltaX = +1; DeltaY = -1; Label = "TopRight" }
      { DeltaX = +1; DeltaY = +0; Label = "Right" }
      { DeltaX = +1; DeltaY = +1; Label = "RightBottom" }
      { DeltaX = +0; DeltaY = +1; Label = "Bottom" }
      { DeltaX = -1; DeltaY = +1; Label = "BottomLeft" }
      { DeltaX = -1; DeltaY = +0; Label = "Left" }
      { DeltaX = -1; DeltaY = -1; Label = "LeftTop" } ]

let substring { Lines = lines; Width = w; Height = h } (startX, startY) { DeltaX = dx; DeltaY = dy } length =
    let endX = startX + dx * (length - 1)
    let endY = startY + dy * (length - 1)
    if endX <= 0 || endX > w ||
       endY <= 0 || endY > h then
        {| Text = ""; Coordinates = ((0, 0), (0, 0)) |}
    else
        let text =
            [| for i in [ 0 .. length-1 ] do
                let x = startX + dx * i
                let y = startY + dy * i
                lines.[y - 1].[x - 1] |]
            |> System.String
        {| Text = text; Coordinates = ((startX, startY), (endX, endY)) |}

let searchWord grid word : (Coordinate * Coordinate) option =
    let length = word |> String.length
    let gridA = grid |> GridA.ofGrid
    seq {
        for x in [ 1 .. gridA.Width ] do
        for y in [ 1 .. gridA.Height ] do
        for direction in directions do
            let candidate = substring gridA (x, y) direction length
            if candidate.Text = word then
                yield candidate.Coordinates }
    |> Seq.tryHead

let search grid wordsToSearchFor : Map<string, (Coordinate * Coordinate) option> =
    wordsToSearchFor
    |> List.map (fun word -> (word, searchWord grid word))
    |> Map.ofList
