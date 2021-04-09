module Hamming

let private incrementDifferences differences a b =
    if a = b then
        differences
    else
        differences + 1

let distance (strand1: string) (strand2: string): int option =
    if strand1.Length = strand2.Length then
        (strand1, strand2)
        ||> Seq.fold2 incrementDifferences 0
        |> Some
    else
        None