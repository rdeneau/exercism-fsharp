module BinarySearch

type private Location = Missing | Found | Left | Right

let find inputs value =
    if inputs <> Array.sort inputs then
        invalidArg (nameof inputs) "Binary search requires that the array is already sorted"

    let locateValueAt index startIndex =
        if value = inputs.[index] then Found
        elif index = startIndex then Missing // No left or right location when the range is flat
        elif value < inputs.[index] then Left
        else Right

    let rec loop startIndex maxIndex =
        let midIndex = (startIndex + maxIndex) / 2
        match locateValueAt midIndex startIndex with
        | Missing -> None
        | Found   -> Some midIndex
        | Left    -> loop startIndex midIndex
        | Right   -> loop (midIndex + 1) maxIndex

    if inputs |> Array.isEmpty then None
    else loop 0 inputs.Length
