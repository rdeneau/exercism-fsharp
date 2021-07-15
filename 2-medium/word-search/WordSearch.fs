module WordSearch

type Coordinate = int * int

let search grid wordsToSearchFor : Map<string, (Coordinate * Coordinate) option> =
    wordsToSearchFor
    |> List.map (fun x -> (x, None))
    |> Map.ofList