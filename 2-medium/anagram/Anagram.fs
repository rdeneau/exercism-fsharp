module Anagram

open System

let private normalize (str: string) =
    str.ToUpperInvariant()
    |> Seq.sort
    |> String.Concat

let private isAnagramOf target source =
    normalize target = normalize source
    && not (String.Equals(source, target, StringComparison.InvariantCultureIgnoreCase))

let findAnagrams sources target =
    sources |> List.filter (isAnagramOf target)
