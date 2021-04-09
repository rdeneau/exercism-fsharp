module Accumulate

/// V1: using List.rev
let accumulateV1 (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec loop rest result =
        match rest with
        | [] -> result
        | head :: tail -> loop tail (func head :: result)

    loop input []
    |> List.rev

/// V2: full implementation
let accumulate (func: 'a -> 'b) (input: 'a list): 'b list =
    let rec loop rest result =
        match rest with
        | [] -> result
        | head :: tail -> loop tail (result @ [ func head ])

    loop input []
