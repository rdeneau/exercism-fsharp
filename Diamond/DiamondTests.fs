module Diamond.Tests

open System.Text.RegularExpressions
open Diamond
open Diamond.Core
open FsCheck.Xunit
open Swensen.Unquote
open Xunit

module Helpers =
    let diamondOfC () =
        [ "  A  "
          " B B "
          "C   C"
          " B B "
          "  A  " ]

    let isTopLeftBottomRightDiagonal (rows: string list) : bool =
        rows
        |> List.mapi (fun i x -> (i, x.IndexOf('X'), x.LastIndexOf('X')))
        |> List.forall (fun (expected, a, b) -> a = b && a = expected)

    let splitHorizontally (rows: string list) =
        let chunkLength = (rows.Length + 1) / 2
        let startChunk  = rows |> List.truncate chunkLength
        let endChunkRev = rows |> List.rev |> List.truncate chunkLength
        (startChunk, endChunkRev)

    let splitVertically (rows: string list) =
        let chunkLength = (rows.[0].Length + 1) / 2
        let startChunk  = rows |> List.map (fun x -> x.Substring(0, chunkLength))
        let endChunkRev = rows |> List.map (fun x -> x.Substring(chunkLength - 1) |> String.rev)
        (startChunk, endChunkRev)

    [<Fact>]
    let ``Test isTopLeftBottomRightDiagonal`` () =
        let rows =
            [ "X  "
              " X "
              "  X" ]
        (isTopLeftBottomRightDiagonal rows) =! true

        let rows =
            [ "X  "
              " X "
              "X X" ]
        (isTopLeftBottomRightDiagonal rows) =! false

    [<Fact>]
    let ``Test splitHorizontally`` () =
        let expected =
            [ "  A  "
              " B B "
              "C   C" ]
        let (startChunk, endChunkRev) = splitHorizontally (diamondOfC ())
        startChunk =! expected
        startChunk =! endChunkRev

    [<Fact>]
    let ``Test splitVertically`` () =
        let expected =
            [ "  A"
              " B "
              "C  "
              " B "
              "  A" ]
        let (startChunk, endChunkRev) = splitVertically (diamondOfC ())
        startChunk =! expected
        startChunk =! endChunkRev

open Helpers

module Properties =
    [<Property>]
    let ``First row contains only the letter A and some spaces`` (letter: Letter) =
        let diamond = diamondOf letter
        diamond
        |> List.head
        |> String.filter (fun c -> c <> ' ')
        = "A"

    [<Property>]
    let ``Trimmed row starts are letters alphabetically sorted`` (letter: Letter) =
        let diamond = diamondOf letter
        let letters =
            diamond
            |> List.map (fun s -> s.TrimStart().[0] |> Letter.ofChar)
            |> List.truncate (letter |> Letter.index |> (+) 1)
        letters = Letter.upTo letter

    [<Property>]
    let ``Diamond is quadratic`` (letter: Letter) =
        let diamond = diamondOf letter
        let expectedLength = diamond.Length
        diamond |> List.forall (fun x -> x.Length = expectedLength)

    [<Property>]
    let ``Diamond has vertical symmetry`` (letter: Letter) =
        let diamond = diamondOf letter
        let (startChunk, endChunk) = splitHorizontally diamond
        startChunk = endChunk

    [<Property>]
    let ``Diamond has horizontal symmetry`` (letter: Letter) =
        let diamond = diamondOf letter
        let (startChunk, endChunk) = splitVertically diamond
        startChunk = endChunk

    [<Property>]
    let ``Second quadrant has letters only on the top-left to bottom-right diagonal`` (letter: Letter) =
        let diamond = diamondOf letter
        let secondQuadrantWithX =
            diamond
            |> splitHorizontally |> fst
            |> List.rev
            |> splitVertically |> fst
            |> List.map (fun x -> Regex.Replace(x, "[A-Z]", "X"))
        secondQuadrantWithX
        |> isTopLeftBottomRightDiagonal

[<Fact>]
let ``Diamond of C is expected figure`` () =
    diamondOf C =! diamondOfC()