module BeerSong

let private ofBeer numberOfBottles =
    match numberOfBottles with
        | 0 -> "no more bottles"
        | 1 -> "1 bottle"
        | n -> $"{(n + 100) % 100} bottles"
    + " of beer"

let private actionWith numberOfBottles =
    match numberOfBottles with
    | 0 -> "Go to the store and buy some more"
    | 1 -> "Take it down and pass it around"
    | _ -> "Take one down and pass it around"

let private verse numberOfBottles =
    [ $"{numberOfBottles |> ofBeer |> String.withCapital} on the wall, {numberOfBottles |> ofBeer}."
      $"{actionWith numberOfBottles}, {numberOfBottles - 1 |> ofBeer} on the wall." ]

let recite (startBottles: int) (takeDown: int) =
    let rec loop verses remainingBottles remainingTakeDown =
        let newVerses = verses @ "" :: verse remainingBottles
        match remainingTakeDown with
        | 1 -> newVerses
        | _ -> loop newVerses (remainingBottles - 1) (remainingTakeDown - 1)

    loop [] startBottles takeDown
    |> List.tail
