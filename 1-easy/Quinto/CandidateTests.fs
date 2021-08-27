module Quinto.CandidateTests

open Swensen.Unquote
open Xunit

[<Fact>]
let ``Create valid candidate with Formula and Result`` () =
    let head = 3
    let tail =
        [ 5, Op.Mul
          4, Op.Add
          2, Op.Mul
          1, Op.Sub ]
    let candidate = Candidate.tryCreate head tail
    let actual = candidate |> Option.map (fun x -> (x.Result, x.Formula))
    actual =! Some (37, "3 * 5 = 15 + 4 = 19 * 2 = 38 - 1 = 37")

[<Fact>]
let ``Fail to create invalid candidate "5 / 8"`` () =
    let actual = Candidate.tryCreate 5 [ 8, Op.Div ]
    actual =! None

[<Fact>]
let ``Fail to create invalid candidate "5 + 1 / 8"`` () =
    let actual = Candidate.tryCreate 5 [ 1, Op.Add; 8, Op.Div ]
    actual =! None

[<Fact>]
let ``Normalize candidate`` () =
    let head = 3
    let tail =
        [ 1, Op.Add
          2, Op.Mul
          4, Op.Mul
          5, Op.Add ]
    let candidate = Candidate.tryCreate head tail
    let actual = candidate |> Option.map (fun x -> (Candidate.normalize x).Formula)
    actual =! Some "1 + 3 = 4 * 2 = 8 * 4 = 32 + 5 = 37"
