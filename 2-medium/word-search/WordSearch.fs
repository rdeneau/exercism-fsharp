module WordSearch

type Coordinate = int * int
type Grid = string list

let searchWord (grid: Grid) (word: string) : (Coordinate * Coordinate) option =
    grid
    |> List.indexed
    |> List.tryPick
        ( fun (y, line) ->
             let x = line.IndexOf word
             if x >= 0
             then Some ((x+1, y+1), (x+word.Length, y+1))
             else None )

let search grid wordsToSearchFor : Map<string, (Coordinate * Coordinate) option> =
    wordsToSearchFor
    |> List.map (fun word -> (word, searchWord grid word))
    |> Map.ofList