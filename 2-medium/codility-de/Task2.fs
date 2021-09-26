module Task2

// 1st solution (translated from C#)
let computeV1 rackBikes =
    let usedRacks = rackBikes |> Set.ofList

    let minUsedRack = usedRacks |> Set.minElement
    let maxUsedRack = usedRacks |> Set.maxElement

    if minUsedRack = maxUsedRack then
        0
    else
        [minUsedRack..maxUsedRack]
        |> List.map (fun x ->
            let isUsed = usedRacks |> Set.contains x
            if isUsed then
                0
            else
                usedRacks
                |> Set.toList
                |> List.map (fun y -> abs (y - x)) // Distance between racks x and y
                |> List.min )
        |> List.max

// More optimised solution
let computeV2 rackBikes =
    let usedRacks =
        rackBikes
        |> List.distinct
        |> List.sort

    let left, right =
        match usedRacks with
        | [] -> -1, 12
        | [x] -> x, x
        | _ ->
            usedRacks
            |> List.pairwise
            |> List.maxBy (fun (x, y) -> y - x)

    (right - left) / 2

// Tests
open Swensen.Unquote
open Xunit

let compute = computeV2

[<Fact>]
let ``Test with [ 5; 5 ]`` () =
    compute [ 5; 5 ] =! 0

[<Fact>]
let ``Test with [ 10; -1 ]`` () =
    compute [ 10; -1 ] =! 5

[<Fact>]
let ``Test with [ 2; 4; 7 ]`` () =
    compute [ 2; 4; 7 ] =! 1

[<Fact>]
let ``Test with [ 2; 4; 8; 10 ]`` () =
    compute [ 2; 4; 8; 10 ] =! 2

(*
// FSI
#time
printfn "V1"
for i in [0..1_000_000] do
    computeV1 [ 2; 4; 8; 10 ] |> ignore
#time
// Réel : 00:00:00.161, Processeur : 00:00:00.171, GC gén0: 60, gén1: 1, gén2: 0

#time
printfn "V2"
for i in [0..1_000_000] do
    computeV2 [ 2; 4; 8; 10 ] |> ignore
#time


// Outputs
--> Minutage activé

V1
Réel : 00:00:01.441, Processeur : 00:00:01.546, GC gén0: 615, gén1: 6, gén2: 2

--> Minutage désactivé


--> Minutage activé

V2
Réel : 00:00:00.509, Processeur : 00:00:00.625, GC gén0: 229, gén1: 2, gén2: 0

--> Minutage désactivé

*)
