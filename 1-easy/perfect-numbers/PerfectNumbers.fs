module PerfectNumbers

type Classification = Perfect | Abundant | Deficient

let private aliquotSumOf n =
    seq { 1 .. n/2 }
    |> Seq.filter (fun factor -> n % factor = 0)
    |> Seq.sum

let private classifyPositive n =
    let sum = aliquotSumOf n
    if   n = sum then Perfect
    elif n > sum then Deficient
    else              Abundant

let classify n =
    if n <= 0 then None
    else classifyPositive n |> Some
