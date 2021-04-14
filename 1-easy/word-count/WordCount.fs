module WordCount

open System

let private unquote (word: string) =
    word.Trim('\'')

let private wordsOf (phrase: string) =
    ([""], phrase.ToLower())
    ||> Seq.fold (fun words c ->
        if Char.IsControl(c) ||
           (Char.IsPunctuation(c) && c <> '\'') ||
           Char.IsSeparator(c) ||
           Char.IsSymbol(c)
        then
            // Current word complete -> Prepare next word (currently empty)
            "" :: words
        else
            // Append char to current word
            $"{List.head words}{c}" :: (List.tail words)
        )
    |> List.filter (String.IsNullOrEmpty >> not)
    |> List.map unquote

let countWords phrase =
    wordsOf phrase
    |> List.countBy id
    |> Map.ofSeq
