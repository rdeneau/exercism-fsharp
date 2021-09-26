module Task3

open System
open Swensen.Unquote
open Xunit

let hasDistinctChars (s: String) =
    if String.IsNullOrEmpty s then
        false
    else
        let s' = s |> Seq.distinct |> Seq.toArray |> String
        s = s'

let combineDistinct (items: string list) =
    let rec loop combinations rest =
        match rest with
        | [] -> combinations
        | x :: rest' ->
            if x |> hasDistinctChars then
                let acc' =
                    [ for y in combinations do
                        yield y
                        let yx = y + x
                        if yx |> hasDistinctChars then
                            yield yx ]
                loop (x :: acc') rest'
            else
                loop combinations rest'
    loop [] items

let tryFindLongestCombination (items: string list) =
    let combinations = combineDistinct items
    if combinations.IsEmpty
    then None
    else Some (combinations |> List.maxBy String.length)

[<Fact>]
let ``"abc" has distinct chars`` () =
    hasDistinctChars "abc" =! true

[<Fact>]
let ``"aaa" has not distinct chars`` () =
    hasDistinctChars "aaa" =! false

[<Fact>]
let ``Find no combination`` () =
    tryFindLongestCombination ["aa"; "bbb"] =! None

[<Fact>]
let ``Find longest combination with distinct chars with [a, b, c]`` () =
    tryFindLongestCombination ["a"; "b"; "c"] =! Some "abc"

[<Fact>]
let ``Find longest combination with distinct chars - Advanced case`` () =
    tryFindLongestCombination ["a"; "ab"; "bb"; ""; "dc"; "egf"; "ja"; "jl"] =! Some "abdcegfjl"
