module BinarySearch

type private Location = NotFound | Found | Left | Right

let find inputs value =
    let locate index inputs =
        if inputs |> Array.isEmpty  then NotFound
        elif value = inputs.[index] then Found
        elif value < inputs.[index] then Left
        else Right

    let rec loop inputs offset =
        let middleIndex = (inputs |> Array.length) / 2
        match inputs |> locate middleIndex with
        | NotFound -> None
        | Found    -> Some (offset + middleIndex)
        | Left     -> loop inputs.[..middleIndex-1] offset
        | Right    -> loop inputs.[middleIndex+1..] (offset + middleIndex + 1)

    loop inputs 0
