module WordSearch

type X = int
type Y = int
type Coordinate = X * Y
type Grid = string list
type Dimension = { Width: int; Height: int }

module Grid =
    let dimensions (grid: Grid) =
        if grid.Length = 0
        then { Width = 0; Height = 0 }
        else { Width = grid.Head.Length; Height = grid.Length }

let reverse (word: string) =
    word
    |> Seq.rev
    |> Seq.toArray
    |> System.String

let transpose (grid: Grid) : Grid =
    grid
    |> Seq.ofList
    |> Seq.transpose
    |> Seq.map (Seq.toArray >> System.String)
    |> Seq.toList

let diagonalsOf (grid: Grid) =
    let { Width = width; Height = height } = grid |> Grid.dimensions
    if width * height = 0 then
        [||]
    else
        let gridA = grid |> List.toArray
        [| for j in [ 1 .. width ] do
            let text =
                [| for x in [ 0 .. width-j ] do
                    let y = j - 1 + x
                    if y < height then gridA.[x].[y] |]
                |> System.String
            {| Text = text; StartY = 0; StartX = j - 1; LeftToRight = true |} |]
        // let ranges =
        //     [ // Diagonals from top edge of the grid
        //       yield!
        //         [| for j in [ 1 .. width ] do
        //             {| iMin = 0
        //                jMin = j - 1
        //                iMax = width - j
        //                jMax = width - 1 |} |]
        //       // Diagonals from left edge of the grid
        //       yield!
        //         [| for i in [ 2 .. height ] do
        //             {| iMin = i - 1
        //                jMin = 0
        //                iMax = height - 1
        //                jMax = height - i |} |] ]

let searchWordInLine y (line: string) (word: string) : (Coordinate * Coordinate) option =
    let x = line.IndexOf word
    if x >= 0
    then Some ((x+1, y), (x+word.Length, y))
    else None

let searchReversedWordInLine y (line: string) (word: string) : (Coordinate * Coordinate) option =
    searchWordInLine (y+1) line (reverse word)
    |> Option.map (fun ((x1, _), (x2, _)) -> ((x2, y), (x1, y)))

let searchWordInGrid (grid: Grid) (word: string) : (Coordinate * Coordinate) option =
    grid
    |> List.indexed
    |> List.tryPick (fun (i, line) ->
        let y = i + 1
        searchWordInLine y line word
        |> Option.orElse (searchReversedWordInLine y line word) )

let searchWordInTransposedGrid grid word =
    searchWordInGrid (transpose grid) word
    |> Option.map (fun ((x1, y1), (x2, y2)) -> ((y1, x1), (y2, x2)))

let searchWordInGridDiagonals grid word =
    let diagonals = diagonalsOf grid
    let gridOfDiagonals = diagonals |> Array.map (fun d -> d.Text) |> Array.toList
    searchWordInGrid gridOfDiagonals word
    |> Option.map (fun ((x, y), _) ->
        let d = diagonals.[y - 1]
        let k = word.Length - 1
        let start = (x + d.StartX,     y + d.StartY)
        let end'' = (x + d.StartX + k, y + d.StartY + k)
        (start, end''))

let searchWord grid word =
    searchWordInGrid grid word
    |> Option.orElse (searchWordInTransposedGrid grid word)
    |> Option.orElse (searchWordInGridDiagonals grid word)

let search grid wordsToSearchFor : Map<string, (Coordinate * Coordinate) option> =
    wordsToSearchFor
    |> List.map (fun word -> (word, searchWord grid word))
    |> Map.ofList
