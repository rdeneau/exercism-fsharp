module BinarySearch

type ComparisonResult = Lower | Equal | Higher

let compare a b =
    if a = b then Equal
    elif a < b then Lower
    else Higher

let find (inputs: int []) (value: int) =
    let rec loop (inputs: int []) offset =
        match inputs.Length with
        | 0 -> None
        | _ ->
            let middleIndex = inputs.Length / 2
            match compare value inputs.[middleIndex] with
            | Equal  -> Some (offset + middleIndex)
            | Lower  -> loop inputs.[..middleIndex-1] offset
            | Higher -> loop inputs.[middleIndex+1..] (offset + middleIndex + 1)
    loop inputs 0