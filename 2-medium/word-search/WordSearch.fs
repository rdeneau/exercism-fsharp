module WordSearch

type X = int
type Y = int
type Coordinate = X * Y
type Grid = string list

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

let searchWord grid word =
    searchWordInGrid grid word
    |> Option.orElse (searchWordInTransposedGrid grid word)

let search grid wordsToSearchFor : Map<string, (Coordinate * Coordinate) option> =
    wordsToSearchFor
    |> List.map (fun word -> (word, searchWord grid word))
    |> Map.ofList
