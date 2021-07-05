module Etl

let private transformOne (score, letters) =
    letters |> List.map (fun letter -> (letter |> System.Char.ToLower, score))

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> =
    scoresWithLetters
    |> Map.toSeq
    |> Seq.collect transformOne
    |> Seq.sortBy fst
    |> Map.ofSeq
