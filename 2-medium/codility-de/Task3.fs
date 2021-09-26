module Task3

open System
open Swensen.Unquote
open Xunit

let combine (items: string list) =
    let rec loop acc rest =
        match rest with
        | [] -> acc
        | x :: xs -> loop ((x :: acc) @ [for y in acc -> y + x]) xs
    loop [] items

let hasDistinctChars (s: String) =
    if String.IsNullOrEmpty s then
        false
    else
        let s' = s |> Seq.distinct |> Seq.toArray |> String
        s = s'

let tryFindLongestCombination (items: string list) =
    let combinations =
        items
        |> List.filter hasDistinctChars
        |> combine
        |> List.filter hasDistinctChars
    if combinations.IsEmpty
    then None
    else Some (combinations |> List.maxBy String.length)

[<Fact>]
let ``Combine 0 item`` () =
    combine [] =! []

[<Fact>]
let ``Combine 1 item`` () =
    combine ["a"] =! ["a"]

[<Fact>]
let ``Combine 2 items`` () =
    combine ["a"; "b"] =! ["b"; "a"; "ab"]

[<Fact>]
let ``Combine 3 items`` () =
    combine ["a"; "b"; "c"] =! ["c"; "b"; "a"; "ab"; "bc"; "ac"; "abc"]

[<Fact>]
let ``"abc" has distinct chars`` () =
    hasDistinctChars "abc" =! true

[<Fact>]
let ``"aaa" has not distinct chars`` () =
    hasDistinctChars "aaa" =! false

[<Fact>]
let ``Find longest combination with distinct chars with [a, b, c]`` () =
    tryFindLongestCombination ["a"; "b"; "c"] =! Some "abc"

[<Fact>]
let ``Find longest combination with distinct chars - Advanced case`` () =
    tryFindLongestCombination ["a"; "ab"; "bb"; ""; "dc"; "egf"; "ja"; "jl"] =! Some "abdcegfjl"
