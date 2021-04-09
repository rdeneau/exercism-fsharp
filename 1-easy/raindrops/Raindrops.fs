module Raindrops

type Sound = Pling | Plang | Plong
type Rule = (int -> bool) * Sound

let isDivisibleBy factor i =
    (i % factor) = 0

let rules: Rule list = [
    (isDivisibleBy 3), Pling
    (isDivisibleBy 5), Plang
    (isDivisibleBy 7), Plong
]

type ConversionResult =
    | Sounds of Sound list
    | Still of int

module ConversionResult =
    let toString result =
        match result with
        | Sounds x -> x |> List.map string |> String.concat "" // ☝ `string` on DU is hacky
        | Still i -> string i

type Converter = int -> ConversionResult

module Converter =
    let ofRule: Rule -> Converter = fun (predicate, word) i ->
        match predicate i with
        | true  -> Sounds [word]
        | false -> Still i

    let combine (f1: Converter) (f2: Converter) : Converter = fun i ->
        match (f1 i), (f2 i) with
        | Sounds x, Sounds y -> Sounds (x @ y)
        | Sounds x, Still _ -> Sounds x
        | Still _, Sounds y -> Sounds y
        | Still _, Still i -> Still i

let convert (number: int) : string =
    let converter =
        rules
        |> List.map Converter.ofRule
        |> List.reduce Converter.combine

    converter number
    |> ConversionResult.toString
